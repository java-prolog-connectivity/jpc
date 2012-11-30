package org.jpc.util.visitor;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.jpc.term.Variable;
import org.jpc.term.visitor.DefaultTermVisitor;

public class CollectVariableNamesVisitor extends DefaultTermVisitor {
	
	Set<String> variableNames;
	
	public CollectVariableNamesVisitor() {
		variableNames = new LinkedHashSet<String>(); //LinkedHashSet to preserve insertion order
	}
	
	@Override
	public void visitVariable(Variable var) {
		variableNames.add(var.name);
	}
	
	
	/**
	 * 
	 * @return a list with all the variable names ordered from left to right in the term
	 */
	public List<String> getVariableNames() {
		return new ArrayList<>(variableNames);
	}

}
