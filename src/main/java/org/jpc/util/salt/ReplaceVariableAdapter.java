package org.jpc.util.salt;

import java.util.Map;

import org.jpc.salt.TermContentHandler;
import org.jpc.salt.TermAdapter;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public class ReplaceVariableAdapter extends TermAdapter {
	private Map<String, ? extends TermConvertable> map;
	
	
	public ReplaceVariableAdapter(TermContentHandler contentHandler, Map<String, ? extends TermConvertable> map) {
		super(contentHandler);
		this.map = map;
	}
	
	@Override
	public TermContentHandler startVariable(String name) {
		TermConvertable termConvertable = map.get(name);
		if(termConvertable != null) {
			Term transformedTerm = termConvertable.asTerm();
			transformedTerm.read(contentHandler);
		}
		else
			super.startVariable(name);
		return this;
	}

}
