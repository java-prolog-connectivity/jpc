package org.jpc.engine.prolog;

import org.jpc.Jpc;
import org.jpc.query.Query;
import org.jpc.term.Term;

public class PrologEngineProxy extends AbstractPrologEngine {

	private PrologEngine proxiedEngine;
	
	public PrologEngineProxy(PrologEngine proxiedEngine) {
		this.proxiedEngine = proxiedEngine;
	}
	
	/**
	 * To allows descendants to instantiate the proxied Prolog engines in a custom way
	 */
	protected PrologEngineProxy() {
	}
	
	protected void setPrologEngine(PrologEngine proxiedEngine) {
		this.proxiedEngine = proxiedEngine;
	}
	
	public PrologEngine getPrologEngine() {
		return proxiedEngine;
	}

	@Override
	public boolean isCloseable() {
		return proxiedEngine.isCloseable();
	}

	@Override
	public ThreadModel threadModel() {
		return proxiedEngine.threadModel();
	}

	@Override
	public Query basicQuery(Term term, boolean errorHandledQuery, Jpc context) {
		return proxiedEngine.basicQuery(term, errorHandledQuery, context);
	}

	@Override
	public Term asTerm(String termString, Jpc context) {
		return proxiedEngine.asTerm(termString, context);
	}

	@Override
	public void close() {
		proxiedEngine.close();
	}

}
