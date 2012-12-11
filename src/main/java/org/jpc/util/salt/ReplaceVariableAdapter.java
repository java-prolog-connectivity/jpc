package org.jpc.util.salt;

import java.util.Map;

import org.jpc.salt.ContentHandler;
import org.jpc.salt.TermAdapter;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public class ReplaceVariableAdapter extends TermAdapter {
	private Map<String, TermConvertable> map;
	
	
	public ReplaceVariableAdapter(ContentHandler contentHandler, Map<String, TermConvertable> map) {
		super(contentHandler);
		this.map = map;
	}
	
	@Override
	public ContentHandler startVariable(String name) {
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
