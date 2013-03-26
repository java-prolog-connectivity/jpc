package org.jpc.query;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class PrologQuery extends Query {

	private PrologEngine prologEngine;
	private Term goal;
	private Jpc context;
	
	
	public PrologQuery(PrologEngine prologEngine, Term goal, Jpc context) {
		this.prologEngine = prologEngine;
		this.goal = goal;
		this.context = context;
	}
	
	@Override
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
	@Override
	public Term getGoal() {
		return goal;
	}
	
	public Jpc getJpcContext() {
		return context;
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return getGoal();
	}
	
	@Override
	public String toString() {
		return goal.toString();
	}

}
