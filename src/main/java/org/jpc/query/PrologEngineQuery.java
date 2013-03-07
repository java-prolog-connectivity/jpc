package org.jpc.query;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class PrologEngineQuery extends Query {

	private Term goal;
	private PrologEngine prologEngine;
	
	
	public PrologEngineQuery(PrologEngine prologEngine, Term term) {
		this.prologEngine = prologEngine;
		this.goal = term;
	}
	
	@Override
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
	@Override
	public Term asTerm() {
		return goal;
	}
	
	protected Term asTerm(String termString) {
		return prologEngine.asTerm(termString);
	}
	
	@Override
	public String toString() {
		return goal.toString();
	}
	
}
