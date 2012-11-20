package org.jpc.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.junit.Test;

public class FloatTermTest {

	@Test
	public void testEquality() {
		assertEquals(new FloatTerm(0), new FloatTerm(0));
		assertEquals(new FloatTerm(-10.5), new FloatTerm(-10.5));
		assertEquals(new FloatTerm(-10.5).hashCode(), new FloatTerm(-10.5).hashCode());
	}
	
	@Test
	public void testArity() {
		assertEquals(new FloatTerm(10.5).arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new FloatTerm(10.5).hasFunctor(new FloatTerm(10.5), 0));
	}
	
}
