package org.jpc.test;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.AbstractTerm;
import org.junit.Test;
import static java.util.Arrays.asList;
import static org.junit.Assert.*;

public class TermTest {
	AbstractTerm t1 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
	
	@Test
	public void testEquality() {
		AbstractTerm t2 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
		assertTrue(t1.equals(t2));
	}
	
	@Test
	public void testStructure() {
		assertTrue(t1.isCompound());
		assertEquals(t1.arity(), 1);
		assertTrue(t1.arg(1).isCompound());
		assertEquals(t1.arg(1).arity(), 1);
		assertTrue(t1.arg(1).arg(1).isAtom());
		assertEquals(t1.arg(1).arg(1).arity(), 0);
	}

}
