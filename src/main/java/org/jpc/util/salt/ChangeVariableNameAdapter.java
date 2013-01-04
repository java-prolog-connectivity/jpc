package org.jpc.util.salt;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.salt.TermContentHandler;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

public class ChangeVariableNameAdapter extends ReplaceVariableAdapter {
	
	public ChangeVariableNameAdapter(TermContentHandler adaptee, Map<String, String> map) {
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
