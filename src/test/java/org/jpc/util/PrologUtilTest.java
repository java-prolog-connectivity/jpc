package org.jpc.util;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.junit.Test;

public class PrologUtilTest {

	@Test
	public void testTermsToSequence() {
		assertTrue(PrologUtil.isSequence(new Compound(",", asList(new Atom("x"), new Atom("x")))));
		assertFalse(PrologUtil.isSequence(new Atom("x")));
	}
	
}
