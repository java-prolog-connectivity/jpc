package org.jpc.term;

import java.util.List;
import java.util.Map;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

/**
 * Implementations of this interface are Java representations of Logic Terms (i.e., Prolog data types)
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public interface Term /*extends TermConvertable*/ {

	/**
	 * 
	 * @return true if the term is a proper Hilog term (i.e., a Hilog term that is not a Prolog term)
	 */
	public abstract boolean isHilog();
	
	/**
	 * Returns the ith argument (if any) of this Term.
	 * Arguments are counted from 1.
	 * throws an ArrayIndexOutOfBoundsException if i is inappropriate.
	 * 
	 * @return the ith argument (counting from one) of this Term.
	 */
	public abstract Term arg(int i);

	/**
	 * Returns the arguments list of this term.
	 * If the term has no arguments will return an empty list
	 * @return the arguments of this term
	 */
	public abstract List<Term> args();

	/**
	 * Returns the arity (i.e., number of arguments) of this Term.
	 * 
	 * @return the arity (1+) of this Term. Returns 0 if the term does not have any arguments (i.e., the Term is an instance of Compound)
	 */
	public abstract int arity();

	public abstract boolean hasFunctor(String nameTermObject, int arity);
	
	/**
	 * whether this term has a functor with a given name and arity
	 * @param nameTermObject the name of this term
	 * @param arity the arity of this term
	 * @return true if the term has the given name and arity. False otherwise
	 */
	public abstract boolean hasFunctor(Term nameTermObject, int arity);

	public abstract boolean hasFunctor(boolean nameTermObject, int arity);

	public abstract boolean hasFunctor(double nameTermObject, int arity);

	public abstract boolean hasFunctor(long nameTermObject, int arity);
	

	/**
	 * whether this Term is a list
	 * 
	 * @return whether this Term is a list
	 */
	public abstract boolean isList();
	
	/**
	 * Returns a list representation of this term. Throws an exception if the term cannot be converted to a list (if it is not either the atom '[]' or a cons compound term)
	 * @return a list representation of this term.
	 */
	public abstract ListTerm asList();

	/**
	 * whether this term does not have unbound variables
	 * @return
	 */
	public abstract boolean isBound();

	/**
	 * the length of this list, iff it is one, else an exception is thrown
	 * 
	 * @throws LException
	 * @return the length (as an int) of this list, iff it is one
	 */
	public abstract int listLength();

	/**
	 * Test if this object is equivalent to the term representation of the object sent as parameter
	 * This is not testing for equality in the mathematical sense, for example:
	 * 		'new Variable("_").equals(new Variable("_"))'
	 * is false, since both the receiver and the arguments are anonymous variables, not the same variable. But:
	 * 		'new Variable("_").termEquals(new Variable("_"))'
	 * is true, since they both have the same term representation
	 * @param termAdaptable
	 * @return
	 */
	public abstract boolean termEquals(Term t);

	/**
	 * Returns a term with all the occurrences of the variables in the parameter map replaced with its associated value (converted to a term)
	 * @param map maps variable names to values.
	 * @return a new term with its variables replaced according to the map
	 */
	public abstract Term replaceVariables(Map<String, ? extends Term> map);

	/**
	 * Replace all the variable names according to the map parameter
	 * @param map maps variable names to new names
	 * @return a new term with its variables renamed according to the map
	 */
	public abstract Term changeVariablesNames(Map<String, String> map);

	/**
	 * Returns the variable names present in the term
	 * @return the variable names present in the term
	 */
	public abstract List<String> getVariablesNames();

	/**
	 * wether the term has a variable with a given name
	 * @param variableName the variable name that is queried
	 * @return wether the term has a variable with a given name
	 */
	public abstract boolean hasVariable(String variableName);

	/**
	 * Returns a list with all the non anonymous variables names
	 * @return a list with all the non anonymous variables names
	 */
	public abstract List<String> nonAnonymousVariablesNames();

	/**
	 * Accepts a Jpc term visitor.
	 * @param termVisitor the accepted visitor
	 */
	public abstract void accept(TermVisitor termVisitor);
	
	/**
	 * Reads the contents of this term (i.e., generates events) to a content handler
	 * @param contentHandler the content handler that will receive the events describing the structure of this term
	 */
	public abstract void read(TermContentHandler contentHandler);
	
	public abstract String toString(PrologEngine prologEngine);
	

}
