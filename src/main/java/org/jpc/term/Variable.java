package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.List;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

/**
 * A class reifying a logic variable
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public final class Variable extends AbstractTerm {

	public static final Variable ANONYMOUS_VAR = new Variable(ANONYMOUS_VAR_NAME);
	
	public static List<Variable> asVariables(Iterable<String> variablesNames) {
		List<Variable> variables = new ArrayList<>();
		for(String variableName : variablesNames) {
			variables.add(new Variable(variableName));
		}
		return variables;
	}
	
//	public static boolean isAnonymousVariableName(String variableName) {
//		return variableName.substring(0, 1).equals("_"); //the variable name is equals to "_" or starts with "_"
//	}
	
	private final String name; // the name of this Variable
	
	public Variable(String name) {
		checkArgument(isValidVariableName(name), "The variable name " + name + " is not valid");
		this.name = name;
	}
	
	public boolean isAnonymous() {
		return termEquals(ANONYMOUS_VAR);
	}
	
	/**
	 * returns the lexical name of this Variable
	 * 
	 * @return the lexical name of this Variable
	 */
	public final String getName() {
		return this.name;
	}
	
	/**
	 * Returns a Prolog source text representation of this Variable
	 * 
	 * @return  a Prolog source text representation of this Variable
	 */
	public String toString() {
		return this.name;
	}
	
	@Override
	public String toEscapedString() {
		return toString();
	}
	
	public static boolean isAnonymousVariableName(String variableName) {
		return variableName.substring(0, 1).equals(ANONYMOUS_VAR_NAME);
	}
	
	/**
	 * Wether a logic variable has a valid name
	 * @param variableName the name of the variable
	 * @return wether a logic variable has a valid name
	 */
	public static boolean isValidVariableName(String variableName) {
		if(variableName.isEmpty())
			return false;
		return isAnonymousVariableName(variableName) || Character.isUpperCase(variableName.toCharArray()[0]); //additional checks could be added here
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	/**
	 * A Variable is equal to another if their names are the same and they are not anonymous.
	 * 
	 * @param   obj  The Object to compare.
	 * @return  true if the Object is a Variable and the above condition apply.
	 */
	@Override
	public boolean equals(Object obj) {
		return obj instanceof Variable && 
				!this.name.equals("_") && 
				this.name.equals(((Variable) obj).name);
	}
	
	@Override
	public final boolean termEquals(Term term) {
		return (term instanceof Variable) && this.name.equals(((Variable) term).name);
	}
	
	@Override
	public boolean hasFunctor(Term nameTerm, int arity) {
		return termEquals(nameTerm) && arity == 0;
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitVariable(this);
	}

	@Override
	public void read(TermContentHandler contentHandler) {
		contentHandler.startVariable(name);
		
	}

	@Override
	public String toString(OperatorsContext operatorsContext) {
		return toString();
	}
	
}
