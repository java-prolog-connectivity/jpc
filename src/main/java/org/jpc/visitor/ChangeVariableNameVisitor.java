package org.jpc.visitor;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

public class ChangeVariableNameVisitor extends ReplaceVariableVisitor {

	public ChangeVariableNameVisitor(Map<String, String> map) {
		super(asVariableReplacementMap(map));
	}
	
	public ChangeVariableNameVisitor(JpcStreamingVisitor adaptee, Map<String, String> map) {
		super(adaptee, asVariableReplacementMap(map));
	}
	
	private static Map<String, TermConvertable> asVariableReplacementMap(final Map<String, String> variableNamesMap) {
		Map<String, TermConvertable> termReplacementMap = new HashMap<String, TermConvertable>() {{
			for(Entry<String, String> entry : variableNamesMap.entrySet()) {
				put(entry.getKey(), new Variable(entry.getValue()));
			}
		}};
		return termReplacementMap;
	}

}
