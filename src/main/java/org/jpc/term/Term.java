package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.UNDERSCORE_VAR_NAME;
import static org.jpc.term.Functor.functor;
import static org.jpc.util.termprocessor.JpcTermCollector.termCollector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.term.compiler.BindableVar;
import org.jpc.term.compiler.Environment;
import org.jpc.term.expansion.DefaultTermExpander;
import org.jpc.term.unification.NonUnifiableException;
import org.jpc.term.visitor.DefaultTermVisitor;
import org.jpc.term.visitor.TermVisitor;
import org.jpc.term.visitor.VariablesCollectorVisitor;
import org.jpc.util.salt.JpcTermStreamer;
import org.jpc.util.salt.TermContentHandler;
import org.jpc.util.salt.adapters.ChangeVariableNameAdapter;
import org.jpc.util.salt.adapters.ReplaceVariableAdapter;
import org.jpc.util.termprocessor.JpcTermCollector;

import java.util.function.Function;

/**
 * Implementations of this interface are Java representations of Prolog Terms (i.e., Prolog data types)
 * Disclaimer: First versions of this class had some methods inspired or taken from the JPL library (specially method comments). 
 * Although the current version has diverged enough to be considered a new implementation, some method comments may still be quite similar, with minor adaptations, as they were originally found in the JPL library.
 * @author scastro
 *
 */
public abstract class Term {

	private Boolean listTerm;
	
	/**
	 * 
	 * @return true if the term is a proper Hilog term (i.e., a Hilog term that is not a Prolog term)
	 */
	public boolean isHilog() { //experimental, may be deleted soon
		return false;
	}
	
	/**
	 * Returns the ith argument (if any) of this Term.
	 * Arguments are counted from 1.
	 * throws an IndexOutOfBoundsException if i is inappropriate.
	 * 
	 * @return the ith argument (counting from one) of this Term.
	 */
	public Term arg(int i) {
		return getArgs().get(i-1);
	}

	/**
	 * Returns the arguments listTerm of this term.
	 * If the term has no arguments will return an empty listTerm
	 * @return the arguments of this term
	 */
	public List<Term> getArgs() {
		return Collections.emptyList(); //assuming no arguments by default
	}

	/**
	 * Returns the arity (i.e., number of arguments) of this Term.
	 * 
	 * @return the arity (1+) of this Term. Returns 0 if the term does not have any arguments (i.e., the Term is an instance of Compound)
	 */
	public int arity() {
		return getArgs().size();
	}

	
	/**
	 * Whether this term has a given functor.
	 * @param functor the functor of this term.
	 * @return true if the term has the given functor. False otherwise.
	 */
	public abstract boolean hasFunctor(Functor functor);

	public boolean hasFunctor(String nameTermObject, int arity) {
		return hasFunctor(functor(nameTermObject, arity));
	}
	
	public boolean hasFunctor(boolean nameTermObject, int arity) {
		return hasFunctor(functor(Boolean.toString(nameTermObject), arity));
	}
	
	public boolean hasFunctor(double nameTermObject, int arity) {
		return hasFunctor(functor(new Float(nameTermObject), arity));
	}
	
	public boolean hasFunctor(long nameTermObject, int arity) {
		return hasFunctor(functor(new Integer(nameTermObject), arity));
	}
	
	/**
	 * whether this Term is a listTerm
	 * 
	 * @return whether this Term is a listTerm
	 */
	public final boolean isList() {
		if(listTerm == null)
			listTerm = basicIsList();
		return listTerm;
	}
	
	protected boolean basicIsList() {
		return false;
	}
	
	/**
	 * Returns a listTerm representation of this term. Throws an exception if the term cannot be converted to a listTerm (if it is not either the atom '[]' or a cons compound term)
	 * @return a listTerm representation of this term.
	 */
	public ListTerm asList() {
		throw new NotAListException(this);
	}

	/**
	 * the length of this listTerm, iff it is one, else an exception is thrown
	 * 
	 * @throws NotAListException
	 * @return the length (as an int) of this listTerm, iff it is one
	 */
	public int listLength() {
		Compound compound = (Compound) this;
		if (compound.hasFunctor(".", 2)) {
			return 1 + compound.arg(2).listLength();
		} else if (compound.hasFunctor("[]", 0)) {
			return 0;
		} else {
			throw new NotAListException(compound);
		}
	}

	/**
	 * 
	 * @return true if the term is ground (it does not have unbound variables). false otherwise.
	 */
	public abstract boolean isGround();
	
	/**
	 * Returns a term with all the occurrences of the variables in the parameter map replaced with its associated value (converted to a term)
	 * @param map maps variable names to values.
	 * @return a new term with its variables replaced according to the map
	 */
	public Term replaceVariables(Map<String, ? extends Term> map) {
		if(isGround())
			return this;
		JpcTermCollector collector = termCollector();
		JpcTermStreamer termWriter = new JpcTermStreamer(collector);
		ReplaceVariableAdapter replaceVariableAdapter = new ReplaceVariableAdapter(termWriter, map);
		read(replaceVariableAdapter);
		return collector.getFirst();
	}

	/**
	 * Replace all the variable names according to the map parameter
	 * @param map maps variable names to new names
	 * @return a new term with its variables renamed according to the map
	 */
	public Term changeVariablesNames(Map<String, String> map) {
		if(isGround())
			return this;
		JpcTermCollector collector = termCollector();
		JpcTermStreamer termWriter = new JpcTermStreamer(collector);
		ChangeVariableNameAdapter changeVariableNameAdapter = new ChangeVariableNameAdapter(termWriter, map);
		read(changeVariableNameAdapter);
		return collector.getFirst();
	}


	/**
	 * whether the term has a variable with a given id
	 * @param variableName the variable id that is queried
	 * @return whether the term has a variable with a given id
	 */
	public boolean hasVariable(String variableName) {
		return getVariableNames().contains(variableName);
	}

	/**
	 * @return the variables names present in the term
	 */
	public List<AbstractVar> getVariables() {
		if(isGround())
			return Collections.emptyList();
		VariablesCollectorVisitor varCollector = new VariablesCollectorVisitor();
		accept(varCollector);
		return varCollector.getVariables();
	}
	
	/**
	 * @return the variables names present in the term
	 */
	public List<String> getVariableNames() {
		return AbstractVar.getVariableNames(getVariables());
	}

	/**
	 * @return a listTerm with all the non underscore variables (i.e., all variables that do not start with "_")
	 */
	public List<AbstractVar> getNonUnderscoreVariables() {
		List<AbstractVar> nonUnderscoreVariables = new ArrayList<>();
		for(AbstractVar var : getVariables()) {
			if(!Var.isUnderscoreVariableName(var.getName()))
				nonUnderscoreVariables.add(var);
		}
		return nonUnderscoreVariables;
	}
	
	/**
	 * @return a listTerm with all the variables that do not start with "_".
	 */
	public List<String> getNonUnderscoreVariableNames() {
		return AbstractVar.getVariableNames(getNonUnderscoreVariables());
	}
	
	/**
	 * Returns a listTerm with all the named variables (i.e., all variables but "_")
	 * @return a listTerm with all the named variables (i.e., all variables but "_")
	 */
	public List<AbstractVar> getNamedVariables() {
		List<AbstractVar> nonUnderscoreVariables = new ArrayList<>();
		for(AbstractVar var : getVariables()) {
			if(!UNDERSCORE_VAR_NAME.equals(var.getName()))
				nonUnderscoreVariables.add(var);
		}
		return nonUnderscoreVariables;
	}
	
	/**
	 * Returns a listTerm with all the named variables names (i.e., all variables but "_")
	 * @return a listTerm with all the named variables names (i.e., all variables but "_")
	 */
	public List<String> getNamedVariablesNames() {
		return AbstractVar.getVariableNames(getNamedVariables());
	}
	
	/**
	 * Accepts a Jpc term visitor.
	 * @param termVisitor the accepted visitor
	 */
	public abstract void accept(TermVisitor termVisitor);
	
	
	public Term termExpansion(Function<Term, Term> termExpander) {
		JpcTermCollector collector = termCollector();
		JpcTermStreamer termWriter = new JpcTermStreamer(collector);
		read(termWriter, termExpander);
		return collector.getFirst();
	}
	
	public void read(TermContentHandler contentHandler) {
		read(contentHandler, new DefaultTermExpander());
	}
	
	public void read(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		Term expandedTerm = termExpander.apply(this);
		if(expandedTerm != null)
			expandedTerm.read(contentHandler);
		else
			basicRead(contentHandler, termExpander);
	}
	
	protected abstract void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander);
	
	//TODO FIXME!
	public boolean subsumes(Term term) {
		return compile().canUnify(term.compile());  //provisional implementation. This is not correct.
	}

	/**
	 * Test if this object is equivalent to the term representation of the object sent as parameter
	 * This is not testing for equality in a mathematical sense, for example:
	 * 		'new Variable("_").equals(new Variable("_"))'
	 * is false, since both the receiver and the arguments are anonymous variables, not the same variable. But:
	 * 		'new Variable("_").termEquals(new Variable("_"))'
	 * is true, since they both have the same term representation
	 * @param term
	 * @return
	 */
	public boolean termEquals(Term term) {
		return equals(term); //default implementation, to be overridden.
	}
	
	/**
	 * @param   list1  a listTerm of Terms
	 * @param   list2  another listTerm of Terms
	 * @return  true if all of the Terms in the (same-length) lists are pairwise term equivalent
	 */
	public static boolean termEquals(List<? extends Term> list1, List<? extends Term> list2) {
		if (list1.size() != list2.size()) {
			return false;
		}
		for (int i = 0; i < list1.size(); ++i) {
			Term term1 = list1.get(i);
			Term term2 = list2.get(i);
			if(!term1.termEquals(term2))
				return false;
		}
		return true;
	}
	
	
	/* ********************************************************************************************************************************
	 * STRING CONVERSION METHODS.
     **********************************************************************************************************************************
     */

	@Override
	public String toString() {
		return toEscapedString();
	}

	public String toEscapedString() {
		return toEscapedString(Dialect.JPC);
	}

	public String toEscapedString(Dialect dialect) {
		return toEscapedString(dialect, OperatorsContext.empty());
	}

	public abstract String toEscapedString(Dialect dialect, OperatorsContext operatorsContext);

	/**
	 * Converts an array of Terms to its escaped String representation.
	 * 
	 * @param   terms    an array of Terms to convert
	 * @return  String representation of an array of Terms
	 */
	public static <T extends Term> String toEscapedString(Dialect dialect, OperatorsContext operatorsContext, T... terms) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < terms.length; ++i) {
			sb.append(terms[i].toEscapedString(dialect, operatorsContext));
			if (i != terms.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	/**
	 * Converts a listTerm of Terms to a String.
	 * 
	 * @param terms
	 * @return String representation of a listTerm of Terms
	 */
	public static <T extends Term> String toEscapedString(Dialect dialect, OperatorsContext operatorsContext, List<T> terms) {
		return toEscapedString(dialect, operatorsContext, terms.toArray(new Term[]{}));
	}
	
	
	/* ********************************************************************************************************************************
	 * UNIFICATION METHODS. 
     **********************************************************************************************************************************
     */

	/**
	 * 
	 * @param term a compiled term
	 * @return true if this term can unify with the parameter term.
	 * @throws UncompiledTermException if this term or the receiver are not compiled and there are unbound variables.
	 */
	public final boolean canUnify(Term term) {
		try {
			unify(term);
			return true;
		} catch(NonUnifiableException e) {
			return false;
		}
	}
	
	/**
	 * @param term a term.
	 * @return a map of variables to terms according to the accomplished unification.
	 */
	public final Map<String, Term> compileAndUnifyVars(Term term) {
		Term thisBindableTerm = compile(true);
		Term thatBindableTerm = term.compile(false);
		thisBindableTerm.unify(thatBindableTerm);
		return thisBindableTerm.varBindings();
	}
	
	/**
	 * @param term a term.
	 * @return this term unified with the term sent as parameter.
	 */
	public final Term compileAndUnify(Term term) {
		Term thisBindableTerm = compile(true);
		Term thatBindableTerm = term.compile(false);
		return thisBindableTerm.unifyAndBind(thatBindableTerm);
	}
	
	/**
	 * @param term
	 * @throws UncompiledTermException if this term or the receiver are not compiled and there are unbound variables.
	 * @throws NonUnifiableException if this term is not unifiable with the receiver.
	 */
	public void unify(Term term) {
		if(term instanceof AbstractVar || term instanceof JRef) { //classes overriding this method should repeat this check.
			term.unify(this);
		} else {
			if(!equals(term))
				throw new NonUnifiableException(this, term);
		}
	}
	
	/**
	 * @return a term where the bindings of the variables of the receiver have been applied.
	 */
	public final Term resolveBindings() {
		return termExpansion(term -> {
			if(term instanceof BindableVar) {
				BindableVar bindableVar = (BindableVar) term;
				if(!bindableVar.isAnonymous())
					return bindableVar.getBinding().resolveBindings();
				else
					return bindableVar.getVar();
			} else if(term.isGround())
				return term;
			return null;
		});
	}
	

	/**
	 * @param term a bindable term.
	 * @return this term unified with the term sent as parameter.
	 */
	public final Term unifyAndBind(Term term) {
		unify(term);
		return resolveBindings();
	}
	

	/**
	 * @return a map of variable names to their unified values.
	 */
	public final Map<String, Term> varBindings() {
		final Map<String, Term> varsMap = new HashMap<>();
		accept(new DefaultTermVisitor() {
			@Override
			public void visitVariable(AbstractVar var) {
				if(var instanceof BindableVar) {
					BindableVar bindableVar = (BindableVar) var;
					if(!bindableVar.isAnonymous()) {
						varsMap.put(bindableVar.getName(), bindableVar.getBinding().resolveBindings());
					}
				}
			}
			
			@Override
			public boolean visitCompound(Compound compound) {
				return !compound.isGround();
			}
		});
		return varsMap;
	}

	
	/* ********************************************************************************************************************************
	 * COMPILATION METHODS.
     **********************************************************************************************************************************
     */
	
	private static final boolean DEFAULT_COMPILATION_NAME_PRESERVATION_POLICY = false;
	
	public final Term compile() {
		return compile(DEFAULT_COMPILATION_NAME_PRESERVATION_POLICY);
	}
	
	public final Term compile(boolean preserveVarNames) {
		return compile(preserveVarNames, new Environment());
	}
	
	public final Term compile(Environment env) {
		return compile(DEFAULT_COMPILATION_NAME_PRESERVATION_POLICY, env);
	}
	
	public final Term compile(boolean preserveVarNames, Environment env) {
		if(preserveVarNames) {
			return prepareForQuery(env);
		} else {
			Term compiledTerm = preCompile(env);
			return compiledTerm.prepareForFrame(env);
		}
	}
	

	/* ********************************************************************************************************************************
	 * INTERNAL COMPILATION METHODS. THE METHODS BELOW ARE NOT INTENDED TO BE DIRECTLY USED BY THE PROGRAMMER.
     **********************************************************************************************************************************
     */
	
	public abstract Term preCompile(Environment env);
	
	public final Term prepareForQuery() {
		return prepareForQuery(new Environment());
	}
	
	public abstract Term prepareForQuery(Environment env);
	
	/**
	 * Method only required for internal usage of the JPC Prolog engine.
	 * Every time a new clause is visited when backtracking, a new frame is created.
	 * @return a term to be used in a new frame.
	 */
	public final Term prepareForFrame() {
		return prepareForFrame(new Environment());
	}
	
	public abstract Term prepareForFrame(Environment env);
	
}
