package org.jpc.query;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.jpc.term.Term;
import org.minitoolbox.CollectionsUtil;

public class ObservableQuery extends QueryAdapter {

	private Collection<QueryListener> listeners;
	
	public ObservableQuery(Query query) {
		this(query, new HashSet<QueryListener>());
	}
	
	public ObservableQuery(Query query, Iterable<QueryListener> listeners) {
		super(query);
		this.listeners = CollectionsUtil.createWeakSet(listeners);
	}

	@Override
	protected void setState(CursorState state) {
		super.setState(state);
		if(listeners != null) { //if listeners == null the object is being initialized. Particularly, the listeners collection has not been initialized yet.
			if(state.equals(CursorState.READY))
				notifyQueryReady();
			else if(state.equals(CursorState.OPEN))
				notifyQueryOpened();
			else if(state.equals(CursorState.EXHAUSTED))
				notifyQueryExhausted();
		}
	}
	
	@Override
	public synchronized boolean hasNext() {
		Boolean hasNext = null;
		try {
			notifyQueryInProgress();
			try {
				hasNext = super.hasNext();
			} catch(Exception e) {
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		return hasNext;
	}
	
	@Override
	public synchronized Map<String,Term> next() {
		Map<String,Term> next = null;
		try {
			notifyQueryInProgress();
			try {
				next = super.next(); //throws a NoSuchElementException in case the query is exhausted
			} catch(Exception e) {
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		notifyNextSolutionFound(next);
		return next;
	}
	
	@Override
	public synchronized Map<String,Term> oneSolution() {
		Map<String,Term> next = null;
		try {
			notifyQueryInProgress();
			try {
				next = super.oneSolution();
			} catch(Exception e) {
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		if(next != null)
			notifyNextSolutionFound(next);
		return next;
	}
	
	@Override
	public synchronized List<Map<String,Term>> solutionsRange(long from, long to) {
		List<Map<String,Term>> allSolutions = null;
		try {
			notifyQueryInProgress();
			try {
				allSolutions = super.solutionsRange(from, to);
			} catch(Exception e) {
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		notifySolutionsFound(allSolutions);
		return allSolutions;
	}
	
	@Override
	public synchronized List<Map<String,Term>> allSolutions() {
		List<Map<String,Term>> allSolutions = null;
		try {
			notifyQueryInProgress();
			try {
				allSolutions = super.allSolutions();
			} catch(Exception e) {
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		notifySolutionsFound(allSolutions);
		return allSolutions;
	}
	
	public void addQueryListener(QueryListener listener) {
		listeners.add(listener);
	}
	
	public void removeQueryListener(QueryListener listener) {
		listeners.remove(listener);
	}
	
	
	
	private void notifyQueryReady() {
		for(QueryListener listener : listeners) {
			listener.onQueryReady();
		}
	}
	
	private void notifyQueryOpened() {
		for(QueryListener listener : listeners) {
			listener.onQueryOpened();
		}
	}
	
	private void notifyQueryExhausted() {
		for(QueryListener listener : listeners) {
			listener.onQueryExhausted();
		}
	}
	
	private void notifyQueryInProgress() {
		for(QueryListener listener : listeners) {
			listener.onQueryInProgress();
		}
	}
	
	private void notifyQueryFinished() {
		for(QueryListener listener : listeners) {
			listener.onQueryFinished();
		}
	}
	
	private void notifyException(Exception e) {
		for(QueryListener listener : listeners) {
			listener.onException(e);
		}
	}
	
	private void notifyNextSolutionFound(Map<String,Term> solution) {
		for(QueryListener listener : listeners) {
			listener.onNextSolutionFound(solution);
		}
	}
	
	private void notifySolutionsFound(List<Map<String,Term>> solutions) {
		for(QueryListener listener : listeners) {
			listener.onSolutionsFound(solutions);
		}
	}

}
