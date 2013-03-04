package org.jpc.exception;

import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;
import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_PREFIX;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.JpcTermWriter;
import org.jpc.salt.TermAdapter;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.Term;

public class MatchExceptionHandler implements ExceptionHandler {

private static final String EXCEPTION_VAR_PREFIX = "JPC_EXCEPTION_UNBOUND_VAR_";
	
	protected Term goal;
	protected Term exceptionTerm;
	
	public MatchExceptionHandler(Term goal, Term exceptionTerm) {
		this.goal = replaceVariables(goal);
		this.exceptionTerm = replaceVariables(exceptionTerm);
	}
	
	public Term getGoal() {
		return goal;
	}

	public Term getExceptionTerm() {
		return exceptionTerm;
	}
	
	private static Term replaceVariables(Term term) {
		JpcTermWriter termWriter = new JpcTermWriter();
		TermAdapter variableAdapter = new TermAdapter(termWriter) {
			@Override
			public TermContentHandler startVariable(String name) {
				String newName = null;
				if(name.equals(ANONYMOUS_VAR_NAME)) {
					newName = ANONYMOUS_VAR_NAME;
				} else if(name.substring(0, 1).equals(ANONYMOUS_VAR_PREFIX)) {
					newName = ANONYMOUS_VAR_PREFIX + EXCEPTION_VAR_PREFIX + name;
				} else {
					newName = EXCEPTION_VAR_PREFIX + name;
				}
				super.startVariable(newName);
				return this;
			}
		};
		term.read(variableAdapter);
		return termWriter.getTerms().get(0);
	}

	public boolean handle(PrologEngine prologEngine, Term goal, Term exceptionTerm) {
		Term unifiedGoal,unifiedExceptionTerm;
		if(this.goal == null) {
			unifiedGoal = goal;
		} else {
			unifiedGoal = prologEngine.unify(this.goal, goal);
			if(unifiedGoal == null)
				return false; //the goal does not unify with the handler goal, so the handler cannot deal with the exception
		}
		if(this.exceptionTerm == null) {
			unifiedExceptionTerm = exceptionTerm;
		} else {
			unifiedExceptionTerm = prologEngine.unify(this.exceptionTerm, exceptionTerm);
			if(unifiedExceptionTerm == null)
				return false; //the goal does not unify with the handler exception term, so the handler cannot deal with the exception
		}
		onMatch(prologEngine, unifiedGoal, unifiedExceptionTerm);
		return true;
	}
	
	public void onMatch(PrologEngine prologEngine, Term unifiedGoal, Term unifiedExceptionTerm) {
		throw new PrologException(unifiedGoal, unifiedExceptionTerm);
	}
	
}
