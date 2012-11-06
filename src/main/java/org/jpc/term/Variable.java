package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

import java.util.Collections;
import java.util.List;

import org.jpc.engine.visitor.AbstractJpcVisitor;

/**
 * A class reifying a logic variable
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Variable in the JPL library.
 * @author scastro
 *
 */
public class Variable extends Term {

	public static final Variable ANONYMOUS_VAR = new Variable("_");
	
	public static boolean isAnonymousVariableName(String variableName) {
		return variableName.substring(0, 1).equals("_"); //the variable name is equals to "_" or starts with "_"
	}
	
	public final String name; // the name of this Variable
	
	public Variable(String name) {
		checkNotNull(name);
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
	
	protected boolean isValidVariableName(String variableName) {
		return !variableName.isEmpty(); //additional checks could be added here
	}
	
	/**
	 * A Variable is equal to another if their names are the same and they are not anonymous.
	 * 
	 * @param   obj  The Object to compare.
	 * @return  true if the Object is a Variable and the above condition apply.
	 */
	public final boolean equals(Object obj) {
		return obj instanceof Variable && !this.name.equals("_") && this.name.equals(((Variable) obj).name);
	}

	@Override
	public final boolean termEquivalent(TermAdaptable termAdaptable) {
		Term term = termAdaptable.asTerm();
		return term.isVariable() && this.name.equals(((Variable) term).name);
	}
	
	@Override
	public boolean hasFunctor(TermAdaptable nameTerm, int arity) {
		return termEquivalent(nameTerm) && arity == 0;
	}

	@Override
	public void accept(AbstractJpcVisitor termVisitor) {
		termVisitor.visitVariable(this);
	}
	
}
