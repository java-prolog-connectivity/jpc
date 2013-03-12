package org.jpc.query;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class PrologEngineQuery extends Query {

	private PrologEngine prologEngine;
	private Term goal;
	private Jpc context;
	
	public PrologEngineQuery(PrologEngine prologEngine, Term term, Jpc context) {
		this.prologEngine = prologEngine;
		this.goal = term;
		this.context = context;
	}
	
	@Override
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
	@Override
	public Term goal() {
		return goal;
	}
	
	public Jpc getJpcContext() {
		return context;
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return goal();
	}
	
	protected Term asTerm(String termString) {
		return prologEngine.asTerm(termString);
	}
	
	@Override
	public String toString() {
		return goal.toString();
	}

	@Override
	protected TermToObjectFunction getTermToObjectFunction(Type targetType) {
		return new TermToObjectFunction(context, targetType);
	}

}
