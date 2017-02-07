package org.jpc.util.salt.adapters;

import java.util.HashMap;
import java.util.Map;

import org.jpc.util.salt.TermContentHandler;
import org.jpc.term.Term;
import org.jpc.term.Var;

public class ChangeVariableNameAdapter extends ReplaceVariableAdapter {
	
	public ChangeVariableNameAdapter(TermContentHandler adaptee, Map<String, String> map) {
		super(adaptee, asVariableReplacementMap(map));
	}
	
	private static Map<String, Term> asVariableReplacementMap(final Map<String, String> variableNamesMap) {
		Map<String, Term> termReplacementMap = new HashMap<String, Term>() {{
			for(Entry<String, String> entry : variableNamesMap.entrySet()) {
				put(entry.getKey(), new Var(entry.getValue()));
			}
		}};
		return termReplacementMap;
	}

}
