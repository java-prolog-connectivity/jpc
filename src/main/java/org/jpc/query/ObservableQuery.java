package org.jpc.query;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jpc.term.Term;
import org.minitoolbox.CollectionsUtil;

public class ObservableQuery extends QueryAdapter {

	private Collection<QueryListener> listeners;
	
	public ObservableQuery(Query query) {
		this(query, new HashSet<QueryListener>());
	}
	
	public ObservableQuery(Query query, Set<QueryListener> listeners) {
		super(query);
		this.listeners = CollectionsUtil.createWeakSet(listeners);
	}

	@Override
	protected void setState(CursorState state) {
		super.setState(state);
		if(state.equals(CursorState.READY))
			notifyOnQueryReady();
		else if(state.equals(CursorState.OPEN))
			notifyOnQueryOpened();
		else if(state.equals(CursorState.EXHAUSTED))
			notifyOnQueryExhausted();
	}
	
	@Override
	public synchronized Map<String,Term> next() {
		Map<String,Term> next = super.next();
		notifyOnNextSolution(next);
		return next;
	}
	
	@Override
	public synchronized List<Map<String,Term>> allSolutions() {
		List<Map<String,Term>> allSolutions = super.allSolutions();
		notifyOnAllSolutions(allSolutions);
		return allSolutions;
	}
	
	
	public void addQueryListener(QueryListener listener) {
		listeners.add(listener);
	}
	
	public void removeQueryListener(QueryListener listener) {
		listeners.remove(listener);
	}
	
	private void notifyOnQueryReady() {
		for(QueryListener listener : listeners) {
			listener.onQueryReady();
		}
	}
	
	private void notifyOnQueryOpened() {
		for(QueryListener listener : listeners) {
			listener.onQueryOpened();
		}
	}
	
	private void notifyOnQueryExhausted() {
		for(QueryListener listener : listeners) {
			listener.onQueryExhausted();
		}
	}
	
	private void notifyOnNextSolution(Map<String,Term> solution) {
		for(QueryListener listener : listeners) {
			listener.onNextSolution(solution);
		}
	}
	
	private void notifyOnAllSolutions(List<Map<String,Term>> solutions) {
		for(QueryListener listener : listeners) {
			listener.onAllSolutions(solutions);
		}
	}

}
