package org.jpc.util.salt;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.jpc.salt.ContentHandler;
import org.jpc.salt.DefaultContentHandler;

public class VariableNamesCollectorHandler extends DefaultContentHandler {
	
	Set<String> variableNames;
	
	public VariableNamesCollectorHandler() {
		variableNames = new LinkedHashSet<String>(); //LinkedHashSet to preserve insertion order
	}
	
	@Override
	public ContentHandler startVariable(String name) {
		variableNames.add(name);
		return this;
	}
	
	/**
	 * 
	 * @return a list with all the variable names ordered from left to right in the term
	 */
	public List<String> getVariableNames() {
		return new ArrayList<>(variableNames);
	}

}
