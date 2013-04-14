package org.jpc.util;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.junit.Test;

public class PrologUtilTest {

	@Test
	public void testTermsToSequence() {
		assertEquals(new Atom("a"), PrologUtil.termSequence(asList(new Atom("a"))));
		assertEquals(new Compound(",", asList(new Atom("a"), new Compound(",", asList(new Atom("b"), new Atom("c"))))), PrologUtil.termSequence(asList(new Atom("a"), new Atom("b"), new Atom("c"))));
	}
	
}
