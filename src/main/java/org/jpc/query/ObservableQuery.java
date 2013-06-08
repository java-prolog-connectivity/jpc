package org.jpc.query;

import static java.util.Arrays.asList;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;

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
	public synchronized QuerySolution next() {
		QuerySolution next = null;
		try {
			notifyQueryInProgress();
			try {
				next = super.next(); //throws a NoSuchElementException in case the query is exhausted
			} catch(NoSuchElementException e) {
				throw e;
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
	public synchronized QuerySolution oneSolutionOrThrow() {
		QuerySolution oneSolution = null;
		try {
			notifyQueryInProgress();
			try {
				oneSolution = super.oneSolutionOrThrow();
			} catch(NoSuchElementException e) {
				throw e;
			} catch(Exception e) { //all other exceptions will generate an event.
				notifyException(e);
				throw e;
			} 
		} finally {
			notifyQueryFinished();
		}
		notifySolutionsFound(asList(oneSolution));
		return oneSolution;
	}
	
	@Override
	public synchronized List<QuerySolution> solutionsRange(long from, long to) {
		List<QuerySolution> allSolutions = null;
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
	public synchronized List<QuerySolution> allSolutions() {
		List<QuerySolution> allSolutions = null;
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

	public synchronized void addQueryListener(QueryListener listener) {
		listeners.add(listener);
	}
	
	public synchronized void removeQueryListener(QueryListener listener) {
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
	
	private void notifyNextSolutionFound(QuerySolution solution) {
		for(QueryListener listener : listeners) {
			listener.onNextSolutionFound(solution);
		}
	}
	
	private void notifySolutionsFound(List<QuerySolution> solutions) {
		for(QueryListener listener : listeners) {
			listener.onSolutionsFound(solutions);
		}
	}

}
