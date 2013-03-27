package org.jpc.query;

import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public class DeterministicPrologQuery extends PrologQuery {

	public DeterministicPrologQuery(PrologEngine prologEngine, Term goal, Jpc context) {
		super(prologEngine, goal, context);
	}

	private List<Map<String, Term>> allSolutions;
	private int index = 0;
	
	private void reset() {
		allSolutions = null;
		index = 0;
	}

	@Override
	protected void basicAbort() {
		reset();
		getPrologEngine().interrupt();
	}

	@Override
	protected void basicClose() {
		reset();
	}

	@Override
	protected void basicRewind() {
		reset();
	}

	@Override
	protected Map<String, Term> basicNext() {
		if(allSolutions == null)
			allSolutions = allSolutions();
		if(index == allSolutions.size())
			throw new NoSuchElementException();
		return allSolutions.get(index++);
	}

}
