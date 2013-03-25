package org.jpc.query;

import java.util.Map;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

import com.google.common.base.Function;

public class QueryAdapter extends Query {

	protected Function<Map<String,Term>, Map<String,Term>> adapterFunction;
	protected Query query;
	
	public QueryAdapter(Query query) {
		this(query, (Function<Map<String, Term>, Map<String, Term>>) CursorAdapter.defaultAdapterFunction);
	}
	
	public QueryAdapter(Query query, Function<Map<String,Term>, Map<String,Term>> adapterFunction) {
		this.query = query;
		this.adapterFunction = adapterFunction;
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
	protected void basicAbort() {
		query.abort();
	}

	@Override
	protected void basicClose() {
		query.close();
	}

	@Override
	protected void basicRewind() {
		query.rewind();
	}

	@Override
	protected Map<String,Term> basicNext() {
		Map<String,Term> adaptee = query.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
}
