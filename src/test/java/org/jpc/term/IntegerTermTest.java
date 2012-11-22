package org.jpc.term;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jpc.term.IntegerTerm;
import org.junit.Test;

public class IntegerTermTest {
	
	@Test
	public void testEquality() {
		assertEquals(new IntegerTerm(0), new IntegerTerm(0));
		assertEquals(new IntegerTerm(-10), new IntegerTerm(-10));
		assertEquals(new IntegerTerm(-10).hashCode(), new IntegerTerm(-10).hashCode());
	}
	
	@Test
	public void testArity() {
		assertEquals(new IntegerTerm(10).arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new IntegerTerm(10).hasFunctor(new IntegerTerm(10), 0));
	}
}
