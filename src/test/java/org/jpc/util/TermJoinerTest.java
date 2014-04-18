package org.jpc.util;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.junit.Test;

public class TermJoinerTest {

	@Test
	public void asSequence() {
		TermJoiner joiner = TermJoiner.getDefault();
		assertEquals(new Atom("a"), joiner.join(new Atom("a")));
		Term sequence = new Compound(",", asList(new Atom("a"), new Compound(",", asList(new Atom("b"), new Atom("c")))));
		assertEquals(sequence, joiner.join(new Atom("a"), new Atom("b"), new Atom("c")));
	}
	
}
