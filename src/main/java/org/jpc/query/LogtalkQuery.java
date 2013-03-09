package org.jpc.query;

import org.jpc.term.Term;

public class LogtalkQuery extends QueryAdapter {

	public LogtalkQuery(Query query) {
		super(query);
	}

	public Term getObjectTerm() {
		return query.goal().arg(1);
	}
	
	public Term getMethodTerm() {
		return query.goal().arg(2);
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return getMethodTerm();
	}
	
}
