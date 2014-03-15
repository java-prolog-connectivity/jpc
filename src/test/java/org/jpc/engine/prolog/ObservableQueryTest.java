package org.jpc.engine.prolog;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

import org.jpc.query.ObservableQuery;
import org.jpc.query.QueryListener;
import org.jpc.query.Solution;
import org.junit.Test;

/**
 * Test the ObservableQuery class
 * @author sergioc
 *
 */
public class ObservableQueryTest {

	@Test
	public void testAllSolutions() {
		ObservableQuery oq;
		QueryListenerRegister listener;
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("true"), Arrays.<QueryListener>asList(listener));
		oq.allSolutions();
		assertTrue(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertFalse(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNull(listener.nextSolution);
		assertEquals(1, listener.solutions.size());
		assertFalse(listener.queryDisposed);
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("false"), Arrays.<QueryListener>asList(listener));
		oq.allSolutions();
		assertTrue(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertFalse(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNull(listener.nextSolution);
		assertEquals(0, listener.solutions.size());
		assertFalse(listener.queryDisposed);
	}

	@Test
	public void testOneSolution() {
		ObservableQuery oq;
		QueryListenerRegister listener;
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("true"), Arrays.<QueryListener>asList(listener));
		oq.oneSolutionOrThrow();
		assertTrue(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertFalse(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNull(listener.nextSolution);
		assertEquals(1, listener.solutions.size());
		assertFalse(listener.queryDisposed);
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("false"), Arrays.<QueryListener>asList(listener));
		try {
			oq.oneSolutionOrThrow();
			fail();
		} catch(NoSuchElementException e) {}
		assertTrue(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertTrue(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNull(listener.nextSolution);
		assertNull(listener.solutions);
		assertFalse(listener.queryDisposed);
	}
	
	
	@Test
	public void testNextSolution() {
		ObservableQuery oq;
		QueryListenerRegister listener;
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("true"), Arrays.<QueryListener>asList(listener));
		oq.next();
		assertFalse(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertFalse(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNotNull(listener.nextSolution);
		assertNull(listener.solutions);
		assertFalse(listener.queryDisposed);
		oq.hasNext();
		assertFalse(listener.queryReady);
		assertTrue(listener.queryExhausted);
		assertNull(listener.exception);
		try {
			oq.next();
			fail();
		} catch(NoSuchElementException e) {}
		assertFalse(listener.queryReady);
		assertNull(listener.exception);
		oq.close();
		assertTrue(listener.queryReady);
		
		
		listener = new QueryListenerRegister();
		oq = new ObservableQuery(defaultPrologEngine().query("false"), Arrays.<QueryListener>asList(listener));
		oq.hasNext();
		assertFalse(listener.queryReady);
		assertTrue(listener.queryOpened);
		assertTrue(listener.queryExhausted);
		assertTrue(listener.queryInProgress);
		assertTrue(listener.queryFinished);
		assertNull(listener.exception);
		assertNull(listener.nextSolution);
		assertNull(listener.solutions);
		assertFalse(listener.queryDisposed);
		try {
			oq.next();
			fail();
		} catch(NoSuchElementException e) {}
		assertFalse(listener.queryReady);
		assertNull(listener.exception);
		oq.close();
		assertTrue(listener.queryReady);
	}
	
	
	private class QueryListenerRegister implements QueryListener {
		public boolean queryReady;
		public boolean queryOpened;
		public boolean queryExhausted;
		public boolean queryInProgress;
		public boolean queryFinished;
		public Exception exception;
		public Solution nextSolution;
		public List<Solution> solutions;
		public boolean queryDisposed;
		
		@Override
		public void onQueryReady() {
			this.queryReady = true;
		}

		@Override
		public void onQueryOpened() {
			this.queryOpened = true;
		}

		@Override
		public void onQueryExhausted() {
			this.queryExhausted = true;
		}

		@Override
		public void onQueryInProgress() {
			this.queryInProgress = true;
		}

		@Override
		public void onQueryFinished() {
			this.queryFinished = true;
		}

		@Override
		public void onException(Exception e) {
			this.exception = e;
		}

		@Override
		public void onNextSolutionFound(Solution solution) {
			this.nextSolution = solution;
		}

		@Override
		public void onSolutionsFound(List<Solution> solutions) {
			this.solutions = solutions;
		}

		@Override
		public void onQueryDisposed() {
			this.queryDisposed = true;
		}
	}

}
