package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.List;

import org.jpc.term.compiled.CompilationContext;

/**
 * A class reifying a logic variable
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public class Var extends AbstractVar {

	public static final Var ANONYMOUS_VAR = new Var(ANONYMOUS_VAR_NAME);
	
	public static boolean isUnderscoreVariableName(String variableName) {
		return variableName.substring(0, 1).equals(ANONYMOUS_VAR_NAME); //the variable id is equals to "_" or starts with "_"
	}
	
	public static List<Var> asVariables(Iterable<String> variablesNames) {
		List<Var> variables = new ArrayList<>();
		for(String variableName : variablesNames) {
			variables.add(new Var(variableName));
		}
		return variables;
	}
	
	/**
	 * Wether a logic variable has a valid id
	 * @param variableName the id of the variable
	 * @return wether a logic variable has a valid id
	 */
	public static boolean isValidVariableName(String variableName) {
		if(variableName.isEmpty())
			return false;
		return isUnderscoreVariableName(variableName) || Character.isUpperCase(variableName.toCharArray()[0]); //additional checks could be added here
	}
	
	
	protected final String name; // the id of this Variable
	
	public Var() {
		this.name = ANONYMOUS_VAR_NAME;
	}
	
	public Var(String name) {
		checkArgument(isValidVariableName(name), "The variable id " + name + " is not valid");
		this.name = name;
	}
	
	@Override
	public boolean isAnonymous() {
		return name.equals(ANONYMOUS_VAR_NAME);
	}
	
	/**
	 * returns the lexical id of this Variable
	 * 
	 * @return the lexical id of this Variable
	 */
	@Override
	public final String getName() {
		return this.name;
	}
	
	
	@Override
	public Term compile(int clauseId, CompilationContext context) {
		return context.compile(this, clauseId);
	}

	@Override
	public Term compileForQuery(CompilationContext context) {
		return context.compileForQuery(this);
	}

	@Override
	public Term forFrame(int frameId) {
		throw new UnsupportedOperationException(); //a call to this method should never happen.
	}
	

	@Override
	public int hashCode() {
		return name.hashCode();
	}
		
	@Override
	public boolean termEquals(Term term) {
		return this == term || (term.getClass().equals(getClass()) && this.name.equals(((Var) term).name));
	}

}
