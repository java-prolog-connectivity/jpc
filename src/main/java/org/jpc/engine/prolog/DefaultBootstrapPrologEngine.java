package org.jpc.engine.prolog;

import org.jpc.query.Query;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;


public abstract class DefaultBootstrapPrologEngine implements BootstrapPrologEngine {

	public boolean stop() {
		throw new UnsupportedOperationException();
	}
	
	public abstract Query query(TermConvertable termConvertable);

	public Term asTerm(String termString) {
		throw new UnsupportedOperationException();
	}

	public String escape(String s) {
		throw new UnsupportedOperationException();
	}
	
}
