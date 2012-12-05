package org.jpc.util.salt;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.jpc.salt.DefaultHandler;

public class VariableNamesCollector extends DefaultHandler {
	
	Set<String> variableNames;
	
	public VariableNamesCollector() {
		variableNames = new LinkedHashSet<String>(); //LinkedHashSet to preserve insertion order
	}
	
	@Override
	public void startVariable(String name) {
		variableNames.add(name);
	}
	
	/**
	 * 
	 * @return a list with all the variable names ordered from left to right in the term
	 */
	public List<String> getVariableNames() {
		return new ArrayList<>(variableNames);
	}

}
