package org.jpc.term;

import static org.jpc.term.Functor.functor;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class FloatTermTest {

	@Test
	public void testEquality() {
		assertEquals(new Float(0), new Float(0));
		assertEquals(new Float(-10.5), new Float(-10.5));
		assertEquals(new Float(-10.5).hashCode(), new Float(-10.5).hashCode());
	}
	
	@Test
	public void testArity() {
		assertEquals(new Float(10.5).arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Float(10.5).hasFunctor(functor(new Float(10.5), 0)));
	}
	
}
