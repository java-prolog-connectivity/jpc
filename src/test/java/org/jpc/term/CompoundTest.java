package org.jpc.term;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.CONS_FUNCTOR;
import static org.jpc.term.Atom.atom;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CompoundTest {
	private final Term t1 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
	private final Compound listCompound = new Compound(CONS_FUNCTOR, asList(atom("a"),
			new Compound(CONS_FUNCTOR, asList(atom("b"),
					new Compound(CONS_FUNCTOR, asList(atom("c"), Atom.NIL))))));


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
		assertEquals(t1.getArity(), 1);
		assertEquals(t1.arg(1).getArity(), 1);
		assertEquals(t1.arg(1).arg(1).getArity(), 0);
	}
	
	@Test
	public void testHasFunctor() {
		assertTrue(t1.hasFunctor("yellow", 1));
		assertFalse(t1.hasFunctor("yellow", 0));
		assertFalse(t1.hasFunctor("yellow", 2));
		assertFalse(t1.hasFunctor("orange", 1));
	}

	@Test
	public void testCompoundListToString() {
		assertEquals("[a, b, c]", listCompound.toString());
	}

}
