package org.jpc.query;

import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

import com.google.common.base.Function;

public class QueryAdapter extends Query {

	protected Function<QuerySolution, QuerySolution> adapterFunction;
	protected Query query;
	
	public QueryAdapter(Query query) {
		this(query, (Function<QuerySolution, QuerySolution>) CursorAdapter.defaultAdapterFunction);
	}
	
	public QueryAdapter(Query query, Function<QuerySolution, QuerySolution> adapterFunction) {
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
	protected Term getDefaultSelectedTerm() {
		return query.getDefaultSelectedTerm();
	}

	@Override
	protected QuerySolution basicOneSolutionOrThrow() {
		QuerySolution adaptee = query.oneSolutionOrThrow();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
	@Override
	protected List<QuerySolution> basicAllSolutions() {
		List<QuerySolution> allSolutions = new ArrayList<>();
		for(QuerySolution adaptee : query.allSolutions()) {
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
	protected QuerySolution basicNext() {
		QuerySolution adaptee = query.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}

}
