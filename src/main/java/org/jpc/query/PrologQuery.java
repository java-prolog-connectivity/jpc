package org.jpc.query;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class PrologQuery extends Query {

	private PrologEngine prologEngine;
	private Term goal;
	private Jpc context;
	
	public PrologQuery(PrologEngine prologEngine, Term term, Jpc context) {
		this.prologEngine = prologEngine;
		this.goal = term;
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
