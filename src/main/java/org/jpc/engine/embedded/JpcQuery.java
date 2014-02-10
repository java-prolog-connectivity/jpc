package org.jpc.engine.embedded;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.jpc.Jpc;
import org.jpc.query.PrologQuery;
import org.jpc.query.Solution;
import org.jpc.term.Term;
import org.jpc.term.unification.NonUnifiableException;

public class JpcQuery extends PrologQuery {
	private Iterator<Clause> clausesIt;
	
	public JpcQuery(JpcEngine prologEngine, Term goal, boolean errorHandledQuery, Jpc context) {
		super(prologEngine, goal, errorHandledQuery, context);
	}
	
	
	@Override
	public boolean isAbortable() {
		return false;
	}
	
	@Override
	protected void basicAbort() {
		throw new UnsupportedOperationException();
	}

	
	@Override
	protected void basicClose() {
		clausesIt = null;
	}

	@Override
	protected List<Solution> basicAllSolutions() {
		return iterativeAllSolutions();
	}
	
	@Override
	public Solution basicNext() {
		if(clausesIt == null) {
			clausesIt = ((JpcEngine)getPrologEngine()).getClauseDatabase().clausesIterator(getGoal()); //change to getInstrumentedGoal();
		}
		while(clausesIt.hasNext()) {
			Clause clause = clausesIt.next();
			Term head = clause.getHead().prepareForFrame();
			Term compiledQuery = getGoal().prepareForQuery();
			Map<String, Term> solutionMap = null;
			try {
				solutionMap = compiledQuery.unifyVarsCompiled(head);
			} catch(NonUnifiableException e) {
				continue;
			}
			Solution querySolution = new Solution(solutionMap, getPrologEngine(), getJpcContext());
			return querySolution;
		} 
		throw new NoSuchElementException();
	}
	
}
