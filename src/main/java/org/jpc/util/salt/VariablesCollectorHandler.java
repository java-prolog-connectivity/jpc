package org.jpc.util.salt;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.jpc.term.AbstractVar;
import org.jpc.term.visitor.DefaultTermVisitor;

public class VariablesCollectorHandler extends DefaultTermVisitor {
	
	Set<AbstractVar> variables; //set no avoid duplicated entries
	
	public VariablesCollectorHandler() {
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
