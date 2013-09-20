package org.jpc.query;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.CATCH;

import org.jpc.Jpc;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public abstract class PrologQuery extends Query {

	private PrologEngine prologEngine;
	private Term goal;
	private Term instrumentedGoal;
	private Jpc context;
	private boolean errorHandledQuery;
	
	public PrologQuery(PrologEngine prologEngine, Term goal, boolean errorHandledQuery, Jpc context) {
		this.prologEngine = prologEngine;
		this.goal = goal;
		this.errorHandledQuery = errorHandledQuery;
		this.context = context;
		instrumentedGoal = instrumentGoal(goal);
	}
	
	public static Term exceptionHandledQueryTerm(Term term) {
		return new Compound(CATCH, asList(term, new Variable(Solution.EXCEPTION_VAR_NAME), Atom.TRUE_TERM));
	}
	
	public static Term withOperatorsQueryTerm(Term term) {
		return new Compound(",", asList(term, OperatorsContext.findAllOperatorsTerm()));
	}
	
	protected Term instrumentGoal(Term goal) {
		goal = withOperatorsQueryTerm(goal);
		if(isErrorHandledQuery()) {
			return exceptionHandledQueryTerm(goal);
		} else {
			return goal;
		}
	}
	
	@Override
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
	@Override
	public boolean isErrorHandledQuery() {
		return errorHandledQuery;
	}
	
	@Override
	public Term getGoal() {
		return goal;
	}
	
	public Term getInstrumentedGoal() {
		return instrumentedGoal;
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
