package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;

import org.jpc.salt.ContentHandler;
import org.jpc.visitor.JpcVisitor;

/**
 * A class reifying a logic variable
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public final class Variable extends AbstractTerm {

	public static final Variable ANONYMOUS_VAR = new Variable("_");
	
	public static boolean isAnonymousVariableName(String variableName) {
		return variableName.substring(0, 1).equals("_"); //the variable name is equals to "_" or starts with "_"
	}
	
	public final String name; // the name of this Variable
	
	public Variable(String name) {
		checkArgument(isValidVariableName(name), "The variable name " + name + " is not valid");
		this.name = name;
	}
	
	/**
	 * returns the lexical name of this Variable
	 * 
	 * @return the lexical name of this Variable
	 */
	public final String name() {
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
	
	/**
	 * Wether a logic variable has a valid name
	 * @param variableName the name of the variable
	 * @return wether a logic variable has a valid name
	 */
	protected boolean isValidVariableName(String variableName) {
		if(variableName.isEmpty())
			return false;
		char firstChar = variableName.toCharArray()[0];
		return firstChar == '_' || Character.isUpperCase(firstChar); //additional checks could be added here
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
	public final boolean termEquals(TermConvertable o) {
		Term term = o.asTerm();
		return term instanceof Variable && this.name.equals(((Variable) term).name);
	}
	
	@Override
	public boolean hasFunctor(TermConvertable nameTerm, int arity) {
		return termEquals(nameTerm) && arity == 0;
	}

	@Override
	public void accept(JpcVisitor termVisitor) {
		termVisitor.visitVariable(this);
	}

	@Override
	public void streamTo(ContentHandler contentHandler) {
		contentHandler.startVariable(name);
		
	}
	
}
