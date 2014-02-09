package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.JpcTermWriter;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.compiled.BindableVar;
import org.jpc.term.compiled.CompilationContext;
import org.jpc.term.expansion.DefaultTermExpander;
import org.jpc.term.visitor.DefaultTermVisitor;
import org.jpc.term.visitor.TermVisitor;
import org.jpc.util.salt.ChangeVariableNameAdapter;
import org.jpc.util.salt.ReplaceVariableAdapter;
import org.jpc.util.salt.VariablesCollectorHandler;

import com.google.common.base.Function;

/**
 * Implementations of this interface are Java representations of Prolog Terms (i.e., Prolog data types)
 * Disclaimer: First versions of this class had some methods inspired or taken from the JPL library (specially method comments). 
 * Although the current version has diverged enough to be considered a new implementation, some method comments may still be quite similar, with minor adaptations, as they were originally found in the JPL library.
 * @author scastro
 *
 */
public abstract class Term {

	private Boolean list;
	
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
	 * Returns the arguments list of this term.
	 * If the term has no arguments will return an empty list
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
		return hasFunctor(new Functor(new Atom(nameTermObject), arity));
	}
	
	public boolean hasFunctor(boolean nameTermObject, int arity) {
		return hasFunctor(new Functor(new Atom(Boolean.toString(nameTermObject)), arity));
	}
	
	public boolean hasFunctor(double nameTermObject, int arity) {
		return hasFunctor(new Functor(new FloatTerm(nameTermObject), arity));
	}
	
	public boolean hasFunctor(long nameTermObject, int arity) {
		return hasFunctor(new Functor(new IntegerTerm(nameTermObject), arity));
	}
	
	/**
	 * whether this Term is a list
	 * 
	 * @return whether this Term is a list
	 */
	public final boolean isList() {
		if(list == null)
			list = basicIsList();
		return list;
	}
	
	protected boolean basicIsList() {
		return false;
	}
	
	/**
	 * Returns a list representation of this term. Throws an exception if the term cannot be converted to a list (if it is not either the atom '[]' or a cons compound term)
	 * @return a list representation of this term.
	 */
	public ListTerm asList() {
		throw new UnsupportedOperationException();
	}

	/**
	 * the length of this list, iff it is one, else an exception is thrown
	 * 
	 * @throws LException
	 * @return the length (as an int) of this list, iff it is one
	 */
	public int listLength() {
		Compound compound = (Compound) this;
		if (compound.hasFunctor(".", 2)) {
			return 1 + compound.arg(2).listLength();
		} else if (compound.hasFunctor("[]", 0)) {
			return 0;
		} else {
			throw new JpcException("term" + compound.toString() + "is not a list");
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
		JpcTermWriter termWriter = new JpcTermWriter();
		ReplaceVariableAdapter replaceVariableAdapter = new ReplaceVariableAdapter(termWriter, map);
		read(replaceVariableAdapter);
		return termWriter.getTerms().get(0);
	}

	/**
	 * Replace all the variable names according to the map parameter
	 * @param map maps variable names to new names
	 * @return a new term with its variables renamed according to the map
	 */
	public Term changeVariablesNames(Map<String, String> map) {
		if(isGround())
			return this;
		JpcTermWriter termWriter = new JpcTermWriter();
		ChangeVariableNameAdapter changeVariableNameAdapter = new ChangeVariableNameAdapter(termWriter, map);
		read(changeVariableNameAdapter);
		return termWriter.getTerms().get(0);
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
		VariablesCollectorHandler varCollector = new VariablesCollectorHandler();
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
	 * @return a list with all the non underscore variables (i.e., all variables that do not start with "_")
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
	 * @return a list with all the variables that do not start with "_".
	 */
	public List<String> getNonUnderscoreVariableNames() {
		return AbstractVar.getVariableNames(getNonUnderscoreVariables());
	}
	
	/**
	 * Returns a list with all the named variables (i.e., all variables but "_")
	 * @return a list with all the named variables (i.e., all variables but "_")
	 */
	public List<AbstractVar> getNamedVariables() {
		List<AbstractVar> nonUnderscoreVariables = new ArrayList<>();
		for(AbstractVar var : getVariables()) {
			if(!ANONYMOUS_VAR_NAME.equals(var.getName()))
				nonUnderscoreVariables.add(var);
		}
		return nonUnderscoreVariables;
	}
	
	/**
	 * Returns a list with all the named variables names (i.e., all variables but "_")
	 * @return a list with all the named variables names (i.e., all variables but "_")
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
		JpcTermWriter termWriter = new JpcTermWriter();
		read(termWriter, termExpander);
		return termWriter.getTerms().get(0);
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
	
	
	/**
	 * Test if this object is equivalent to the term representation of the object sent as parameter
	 * This is not testing for equality in a mathematical sense, for example:
	 * 		'new Variable("_").equals(new Variable("_"))'
	 * is false, since both the receiver and the arguments are anonymous variables, not the same variable. But:
	 * 		'new Variable("_").termEquals(new Variable("_"))'
	 * is true, since they both have the same term representation
	 * @param termAdaptable
	 * @return
	 */
	public boolean termEquals(Term t) {
		return equals(t); //default implementation, to be overridden.
	}
	
	/**
	 * @param   list1  a list of Terms
	 * @param   list2  another list of Terms
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
	
	/**
	 * Reads the contents of this term (i.e., generates events) to a content handler
	 * @param contentHandler the content handler that will receive the events describing the structure of this term
	 */
	@Override
	public String toString() {
		return toEscapedString();
	}
	
	public abstract String toEscapedString();
	
	public abstract String toString(OperatorsContext operatorsContext);


	/**
	 * Converts an array of Terms to its escaped String representation.
	 * 
	 * @param   terms    an array of Terms to convert
	 * @return  String representation of an array of Terms
	 */
	public static <T extends Term> String toEscapedString(T... terms) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < terms.length; ++i) {
			sb.append(terms[i].toEscapedString());
			if (i != terms.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param terms
	 * @return String representation of a list of Terms
	 */
	public static <T extends Term> String toEscapedString(List<T> terms) {
		return toEscapedString(terms.toArray(new Term[]{}));
	}
	
	
	/* ********************************************************************************************************************************
	 * UNIFICATION METHODS. 
     **********************************************************************************************************************************
     */

	private static final int NO_CLAUSE_CODE = -1;

	/**
	 * @param term a term.
	 * @return a map of variables to terms according to the accomplished unification.
	 */
	public final Map<String, Term> unifyVars(Term term) {
		Term thisBindableTerm = prepareForQuery();
		Term thatBindableTerm = term.compile(NO_CLAUSE_CODE).prepareForFrame();
		return thisBindableTerm.unifyVarsCompiled(thatBindableTerm);
	}
	
	/**
	 * @param term a term.
	 * @return this term unified with the term sent as parameter.
	 */
	public final Term unify(Term term) {
		Term thisBindableTerm = prepareForQuery();
		Term thatBindableTerm = term.compile(NO_CLAUSE_CODE).prepareForFrame();
		return thisBindableTerm.unifyCompiled(thatBindableTerm);
	}
	
	
	/**
	 * THE METHODS BELOW ARE NOT INTENDED TO BE DIRECTLY USED BY THE PROGRAMMER.
	 */
	
	/**
	 * @param term a bindable term.
	 * @return a map of variables to terms according to the accomplished unification.
	 */
	public final Map<String, Term> unifyVarsCompiled(Term term) {
		doUnification(term);
		return unifiedVars();
	}

	/**
	 * @param term a bindable term.
	 * @return this term unified with the term sent as parameter.
	 */
	public final Term unifyCompiled(Term term) {
		doUnification(term);
		return resolveBindings();
	}
	
	public void doUnification(Term term) {
		if(term instanceof AbstractVar || term instanceof JRef) {
			term.doUnification(this);
		} else {
			if(!equals(term))
				throw new NonUnifiableException(this, term);
		}
	}
	
	public final Term resolveBindings() {
		return termExpansion(new Function<Term, Term>() {
			public Term apply(Term term) {
				if(term instanceof BindableVar) {
					BindableVar bindableVar = (BindableVar) term;
					if(!bindableVar.isAnonymous())
						return bindableVar.getBinding().resolveBindings();
					else
						return bindableVar.getVar();
				}
				return null;
			}
		});
	}
	
	public final Map<String, Term> unifiedVars() {
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
		});
		return varsMap;
	}
	
	/* ********************************************************************************************************************************
	 * COMPILATION METHODS. THE METHODS BELOW ARE NOT INTENDED TO BE DIRECTLY USED BY THE PROGRAMMER.
     **********************************************************************************************************************************
     */
	
	public final Term compile(int clauseId) {
		return compile(clauseId, new CompilationContext());
	}
	
	public abstract Term compile(int clauseId, CompilationContext context);
	
	public final Term prepareForQuery() {
		return prepareForQuery(new CompilationContext());
	}
	
	public abstract Term prepareForQuery(CompilationContext context);
	
	/**
	 * Method only required for internal usage of the JPC Prolog engine.
	 * Every time a new clause is visited when backtracking, a new frame is created.
	 * @return a term to be used in a new frame.
	 */
	public final Term prepareForFrame() {
		return prepareForFrame(new CompilationContext());
	}
	
	public abstract Term prepareForFrame(CompilationContext context);
	
}
