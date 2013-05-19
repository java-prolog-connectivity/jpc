package org.jpc.query;

import java.util.List;
import java.util.NoSuchElementException;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public abstract class DeterministicPrologQuery extends PrologQuery {

	public DeterministicPrologQuery(PrologEngine prologEngine, Term goal, Jpc context) {
		super(prologEngine, goal, context);
	}

	private List<QuerySolution> allSolutions;
	private int index = 0;
	
	private void reset() {
		allSolutions = null;
		index = 0;
	}

	@Override
	protected void basicAbort() {
		reset();
	}

	@Override
	protected void basicClose() {
		reset();
	}

	@Override
	protected QuerySolution basicNext() {
		if(allSolutions == null)
			allSolutions = basicAllSolutions();
		if(index == allSolutions.size())
			throw new NoSuchElementException();
		return allSolutions.get(index++);
	}

}
