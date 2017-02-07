package org.jpc.util.salt.adapters;

import java.util.Map;

import org.jpc.util.salt.TermContentHandler;
import org.jpc.util.salt.TermAdapter;
import org.jpc.term.Term;

public class ReplaceVariableAdapter extends TermAdapter {
	private Map<String, ? extends Term> map;
	
	
	public ReplaceVariableAdapter(TermContentHandler contentHandler, Map<String, ? extends Term> map) {
		super(contentHandler);
		this.map = map;
	}
	
	@Override
	public TermContentHandler startVariable(String name) {
		Term term = map.get(name);
		if(term != null) {
			term.read(contentHandler);
		}
		else
			super.startVariable(name);
		return this;
	}

}
