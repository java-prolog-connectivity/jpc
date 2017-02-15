package org.jpc.term;

import static org.jpc.term.Functor.functor;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class IntegerTermTest {
	
	@Test
	public void testEquality() {
		assertEquals(new Integer(0), new Integer(0));
		assertEquals(new Integer(-10), new Integer(-10));
		assertEquals(new Integer(-10).hashCode(), new Integer(-10).hashCode());
	}
	
	@Test
	public void testArity() {
		assertEquals(new Integer(10).arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Integer(10).hasFunctor(functor(new Integer(10), 0)));
	}
}
