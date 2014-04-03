package org.jpc.engine.logtalk;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;

import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class LogtalkSideApiTest {

	@Test
	public void testReturningTerm() {
		Term term;
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::invoke(get('a'))").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::get('a')").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
		term = defaultPrologEngine().query("[a:1,b:2]::get('a') returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}

}
