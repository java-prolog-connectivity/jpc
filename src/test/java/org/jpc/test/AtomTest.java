package org.jpc.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.jpc.term.Atom;
import org.jpc.term.Variable;
import org.junit.Test;

public class AtomTest {

	@Test
	public void testEquality() {
		assertEquals(new Atom("hello"), new Atom("hello"));
		assertEquals(new Atom("hello").hashCode(), new Atom("hello").hashCode());
		assertFalse(new Atom("hello").equals(new Variable("_")));
		assertFalse(new Atom("Hello").equals(new Variable("Hello")));
	}
	
	@Test
	public void testArity() {
		assertEquals(new Atom("hello").arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(new Atom("hello").hasFunctor(new Atom("hello"), 0));
	}

}
