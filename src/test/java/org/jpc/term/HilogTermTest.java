package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class HilogTermTest {

	@Test
	public void isHilog() {
		assertTrue(new Compound(new Variable("_"), asList(new Atom("atom"))).isHilog());
		assertTrue(new Compound(new Variable("V"), asList(new Atom("atom"))).isHilog());
		assertTrue(new Compound(new IntegerTerm(1), asList(new Atom("atom"))).isHilog());
		assertTrue(new Compound(new FloatTerm(1), asList(new Atom("atom"))).isHilog());
		assertTrue(new Compound(new Compound("c", asList(new Atom("atom"))), asList(new Atom("atom"))).isHilog());
	}
	
	@Test
	public void isNotHilog() {
		assertFalse(new Variable("_").isHilog());
		assertFalse(new Variable("V").isHilog());
		assertFalse(new Atom("atom").isHilog());
		assertFalse(new IntegerTerm(1).isHilog());
		assertFalse(new FloatTerm(1).isHilog());
		assertFalse(new Compound("x", asList(new Atom("atom"))).isHilog());
		assertFalse(new Compound(new Atom("x"), asList(new Atom("atom"))).isHilog());
	}
	
}
