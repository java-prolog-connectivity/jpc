package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.JpcTermWriter;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.expansion.DummyTermExpander;
import org.jpc.term.expansion.TermExpander;
import org.jpc.term.visitor.TermVisitor;
import org.jpc.util.salt.ChangeVariableNameAdapter;
import org.jpc.util.salt.ReplaceVariableAdapter;
import org.jpc.util.salt.VariableNamesCollectorHandler;

import com.google.common.base.Optional;

/**
 * Implementations of this interface are Java representations of Prolog Terms (i.e., Prolog data types)
 * Disclaimer: First versions of this class had some methods inspired or taken from the JPL library (specially method comments). 
 * Although the current version has diverged enough to be considered a new implementation, some method comments may still be quite similar, with minor adaptations, as they were originally found in the JPL library.
 * @author scastro
 *
 */
public abstract class Term {

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

	public boolean hasFunctor(String nameTermObject, int arity) {
		return hasFunctor(new Atom(nameTermObject), arity);
	}
	
	/**
	 * whether this term has a functor with a given name and arity
	 * @param nameTermObject the name of this term
	 * @param arity the arity of this term
	 * @return true if the term has the given name and arity. False otherwise
	 */
	public abstract boolean hasFunctor(Term nameTermObject, int arity);

	public boolean hasFunctor(boolean nameTermObject, int arity) {
		return hasFunctor(new Atom(Boolean.toString(nameTermObject)), arity);
	}
	
	public boolean hasFunctor(double nameTermObject, int arity) {
		return hasFunctor(new FloatTerm(nameTermObject), arity);
	}
	
	public boolean hasFunctor(long nameTermObject, int arity) {
		return hasFunctor(new IntegerTerm(nameTermObject), arity);
	}
	

	/**
	 * whether this Term is a list
	 * 
	 * @return whether this Term is a list
	 */
	public boolean isList() {
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
	 * whether this term does not have unbound variables
	 * @return
	 */
	public boolean isBound() {
		return getVariablesNames().isEmpty();
	}

	/**
	 * Returns a term with all the occurrences of the variables in the parameter map replaced with its associated value (converted to a term)
	 * @param map maps variable names to values.
	 * @return a new term with its variables replaced according to the map
	 */
	public Term replaceVariables(Map<String, ? extends Term> map) {
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
		JpcTermWriter termWriter = new JpcTermWriter();
		ChangeVariableNameAdapter changeVariableNameAdapter = new ChangeVariableNameAdapter(termWriter, map);
		read(changeVariableNameAdapter);
		return termWriter.getTerms().get(0);
	}


	/**
	 * whether the term has a variable with a given name
	 * @param variableName the variable name that is queried
	 * @return whether the term has a variable with a given name
	 */
	public boolean hasVariable(String variableName) {
		return getVariablesNames().contains(variableName);
	}

	/**
	 * Returns the variables names present in the term
	 * @return the variables names present in the term
	 */
	public List<Variable> getVariables() {
		return Variable.asVariables(getVariablesNames());
	}
	
	/**
	 * Returns the variables names present in the term
	 * @return the variables names present in the term
	 */
	public List<String> getVariablesNames() {
		VariableNamesCollectorHandler variableNamesCollector = new VariableNamesCollectorHandler();
		read(variableNamesCollector);
		return variableNamesCollector.getVariableNames();
	}

	/**
	 * Returns a list with all the non anonymous variables (i.e., all variables that do not start with "_")
	 * @return a list with all the non anonymous variables (i.e., all variables that do not start with "_")
	 */
	public List<Variable> getNonAnonymousVariables() {
		return Variable.asVariables(getNamedVariablesNames());
	}
	
	/**
	 * Returns a list with all the non anonymous variables names (i.e., all variables that do not start with "_")
	 * @return a list with all the non anonymous variables names (i.e., all variables that do not start with "_")
	 */
	public List<String> getNonAnonymousVariablesNames() {
		List<String> nonAnonymousVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!Variable.isAnonymousVariableName(variableName))
				nonAnonymousVariablesNames.add(variableName);
		}
		return nonAnonymousVariablesNames;
	}
	
	/**
	 * Returns a list with all the named variables (i.e., all variables but "_")
	 * @return a list with all the named variables (i.e., all variables but "_")
	 */
	public List<Variable> getNamedVariables() {
		return Variable.asVariables(getNamedVariablesNames());
	}
	
	/**
	 * Returns a list with all the named variables names (i.e., all variables but "_")
	 * @return a list with all the named variables names (i.e., all variables but "_")
	 */
	public List<String> getNamedVariablesNames() {
		List<String> namedVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!variableName.equals(ANONYMOUS_VAR_NAME))
				namedVariablesNames.add(variableName);
		}
		return namedVariablesNames;
	}
	
	/**
	 * Accepts a Jpc term visitor.
	 * @param termVisitor the accepted visitor
	 */
	public abstract void accept(TermVisitor termVisitor);
	
	public Term termExpansion(Term term, TermExpander termExpander) {
		JpcTermWriter termWriter = new JpcTermWriter();
		read(termWriter, termExpander);
		return termWriter.getTerms().get(0);
	}
	
	public void read(TermContentHandler contentHandler) {
		read(contentHandler, new DummyTermExpander());
	}
	
	public void read(TermContentHandler contentHandler, TermExpander termExpander) {
		Optional<Term> optExpansion = termExpander.expand(this);
		if(optExpansion.isPresent())
			optExpansion.get().read(contentHandler);
		else
			basicRead(contentHandler, termExpander);
	}
	
	protected abstract void basicRead(TermContentHandler contentHandler, TermExpander termExpander);
	
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
	
}
