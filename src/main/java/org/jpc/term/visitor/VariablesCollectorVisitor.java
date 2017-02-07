package org.jpc.term.visitor;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.jpc.term.AbstractVar;

public class VariablesCollectorVisitor extends DefaultTermVisitor {
	
	Set<AbstractVar> variables; //set no avoid duplicated entries
	
	public VariablesCollectorVisitor() {
		variables = new LinkedHashSet<>(); //LinkedHashSet to preserve insertion order
	}
	
	@Override
	public void visitVariable(AbstractVar var) {
		variables.add(var);
	}
	
	/**
	 * 
	 * @return a list with all the variable names ordered from left to right in the term
	 */
	public List<AbstractVar> getVariables() {
		return new ArrayList<>(variables);
	}

}
