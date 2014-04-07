package org.jpc.query;

import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

import com.google.common.base.Function;
import com.google.common.base.Functions;

public class QueryAdapter extends Query {

	protected Function<Solution, Solution> adapterFunction;
	protected Query query;
	
	public QueryAdapter(Query query) {
		this(query, Functions.<Solution>identity());
	}
	
	public QueryAdapter(Query query, Function<Solution, Solution> adapterFunction) {
		this.query = query;
		this.adapterFunction = adapterFunction;
	}

	public boolean isAbortable() {
		return query.isAbortable();
	}
	
	@Override
	public Jpc getJpcContext() {
		return query.getJpcContext();
	}
	
	@Override
	public PrologEngine getPrologEngine() {
		return query.getPrologEngine();
	}
	
	@Override
	public Term getGoal() {
		return query.getGoal();
	}
	
	@Override
	public boolean isErrorHandledQuery() {
		return query.isErrorHandledQuery();
	}
	
	@Override
	protected boolean shouldVerifySolution() {
		return false;
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return query.getDefaultSelectedTerm();
	}
	
	@Override
	protected Solution basicOneSolutionOrThrow() {
		Solution adaptee = query.oneSolutionOrThrow();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
	@Override
	protected List<Solution> basicAllSolutions() {
		List<Solution> allSolutions = new ArrayList<>();
		for(Solution adaptee : query.allSolutions()) {
			allSolutions.add(adapterFunction.apply(adaptee));
		}
		return allSolutions;
	}
	
	@Override
	protected void basicAbort() {
		query.abort();
	}

	@Override
	protected void basicClose() {
		query.close();
	}

	@Override
	protected Solution basicNext() {
		Solution adaptee = query.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}

}
