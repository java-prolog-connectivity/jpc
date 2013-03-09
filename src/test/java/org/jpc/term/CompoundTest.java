package org.jpc.term;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.jpc.util.LogicUtil;
import org.junit.Test;

public class CompoundTest {
	Term t1 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
	
	@Test
	public void testEquality() {
		Term t2 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
		assertEquals(t1, t2);
		assertEquals(t1.hashCode(), t2.hashCode());
		Term t3 = new Compound("orange", asList(new Compound("blue", asList(new Atom("red")))));
		assertFalse(t1.equals(t3));
		Term t4 = new Compound("yellow", asList(new Compound("orange", asList(new Atom("red")))));
		assertFalse(t1.equals(t4));
		Term t5 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("orange")))));
		assertFalse(t1.equals(t5));
	}
	
	@Test
	public void testArity() {
		assertEquals(t1.arity(), 1);
		assertEquals(t1.arg(1).arity(), 1);
		assertEquals(t1.arg(1).arg(1).arity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(t1.hasFunctor("yellow", 1));
		assertFalse(t1.hasFunctor("yellow", 0));
		assertFalse(t1.hasFunctor("yellow", 2));
		assertFalse(t1.hasFunctor("orange", 1));
	}
	
	@Test
	public void sequence() {
		Term sequence = new Compound(",", asList(new Atom("a"), new Compound(",", asList(new Atom("b"), new Atom("c")))));
		assertEquals(3, LogicUtil.sequenceLength(sequence));
		List<Term> terms = LogicUtil.sequenceAsTerms(sequence);
		assertEquals(asList(new Atom("a"), new Atom("b"), new Atom("c")), terms);
		assertEquals(sequence, LogicUtil.termSequence(terms));
	}

}
