package org.jpc.query;

import java.lang.reflect.Type;
import java.util.Map;

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
	public PrologEngine getPrologEngine() {
		return query.getPrologEngine();
	}
	
	@Override
	public Term goal() {
		return query.goal();
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return query.getDefaultSelectedTerm();
	}
	
	@Override
	protected Term asTerm(String termString) {
		return query.asTerm(termString);
	}

	@Override
	public boolean hasNext() {
		return query.hasNext();
	}

	@Override
	public Map<String, Term> next() {
		return adapterFunction.apply(query.next());
	}

	@Override
	public boolean isOpen() {
		return query.isOpen();
	}

	@Override
	public void abort() {
		query.abort();
	}

	@Override
	public void close() {
		 query.close();
	}

	@Override
	protected TermToObjectFunction getTermToObjectFunction(Type targetType) {
		return query.getTermToObjectFunction(targetType);
	}
	
}
