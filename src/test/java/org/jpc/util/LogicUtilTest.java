package org.jpc.util;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.junit.Test;

public class LogicUtilTest {

	@Test
	public void testTermsToSequence() {
		assertEquals(new Atom("a"), LogicUtil.termsToSequence(asList(new Atom("a"))));
		assertEquals(new Compound(",", asList(new Atom("a"), new Compound(",", asList(new Atom("b"), new Atom("c"))))), LogicUtil.termsToSequence(asList(new Atom("a"), new Atom("b"), new Atom("c"))));
	}
	
}
