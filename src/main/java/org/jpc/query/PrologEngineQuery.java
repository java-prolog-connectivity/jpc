package org.jpc.query;

import org.jpc.converter.TermConvertable;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class PrologEngineQuery extends Query {

	private Term goal;
	private PrologEngine prologEngine;
	
	
	public PrologEngineQuery(PrologEngine prologEngine, TermConvertable termConvertable) {
		this.prologEngine = prologEngine;
		this.goal = termConvertable.asTerm();
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
