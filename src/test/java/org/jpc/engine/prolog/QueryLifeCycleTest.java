package org.jpc.engine.prolog;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.NoSuchElementException;

import org.jpc.query.Query;
import org.junit.Test;

/**
 * Test the life cycle of Prolog queries
 * @author sergioc
 *
 */
public class QueryLifeCycleTest {

	@Test
	public void testNextAlone() {
		Query q = defaultPrologEngine().query("true");
		assertNotNull(q.next());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
	}
	
	@Test
	public void testNextAfterOneSolution() {
		Query q = defaultPrologEngine().query("true");
		assertNotNull(q.oneSolutionOrThrow());
		q.next();
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
		
		q = defaultPrologEngine().query("false");
		try {
			q.oneSolutionOrThrow();
			fail();
		} catch(NoSuchElementException e) {}
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
	}
	
	@Test
	public void testNextAfterAllSolutions() {
		Query q = defaultPrologEngine().query("true");
		assertNotNull(q.allSolutions());
		q.next();
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
		
		q = defaultPrologEngine().query("false");
		assertTrue(q.allSolutions().isEmpty());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
	}
	
	@Test
	public void testHasNext() {
		Query q = defaultPrologEngine().query("true");
		assertTrue(q.hasNext());
		assertTrue(q.hasNext());
		assertNotNull(q.next());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
		assertFalse(q.hasNext());
		assertFalse(q.hasNext());
		
	}
	
	@Test
	public void testOneAndAllSolutions() {
		Query q = defaultPrologEngine().query("true");
		assertTrue(q.hasSolution());
		assertTrue(q.hasSolution());
		assertEquals(1, q.allSolutions().size());
		assertEquals(1, q.allSolutions().size());
		assertTrue(q.hasSolution());
		
		q = defaultPrologEngine().query("false");
		assertFalse(q.hasSolution());
		assertFalse(q.hasSolution());
		assertEquals(0, q.allSolutions().size());
		assertEquals(0, q.allSolutions().size());
		assertFalse(q.hasSolution());
	}
	
	@Test
	public void testInvalidState() {
		Query q = defaultPrologEngine().query("true");
		assertTrue(q.hasNext()); //opens the cursor
		try {
			q.hasSolution();
			fail();
		} catch(IllegalStateException e) {}
		try {
			q.oneSolutionOrThrow();
			fail();
		} catch(IllegalStateException e) {}
		try {
			q.allSolutions();
			fail();
		} catch(IllegalStateException e) {}
		assertNotNull(q.next());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
	}
	
	@Test
	public void testClose() {
		Query q = defaultPrologEngine().query("true");
		q.close();
		assertTrue(q.hasNext());
		q.close();
		assertTrue(q.hasNext());
		assertNotNull(q.next());
		assertFalse(q.hasNext());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
		q.close();
		assertNotNull(q.next());
		try {
			q.next();
			fail();
		} catch(NoSuchElementException e) {}
	}
	
}
