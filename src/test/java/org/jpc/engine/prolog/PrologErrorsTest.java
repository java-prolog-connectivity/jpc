package org.jpc.engine.prolog;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.jpc.error.PrologParsingException;
import org.jpc.term.Atom;
import org.junit.Test;

public class PrologErrorsTest {

	@Test
	public void testPrologError() {
		try {
			defaultPrologEngine().query("throw(dummy_error)").hasSolution(); //Arguments are not sufficiently instantiated
			fail();
		} catch(org.jpc.error.PrologError e){
			assertEquals(new Atom("dummy_error"), e.asTerm());
		}
	}
	
	@Test
	public void testInstantiationError() {
		try {
			defaultPrologEngine().query("A is A").hasSolution(); //Arguments are not sufficiently instantiated
			fail();
		} catch(org.jpc.error.InstantiationError e){
		}
	}

	@Test
	public void testSyntaxError() {
		try {
			defaultPrologEngine().query("x++y").hasSolution(); //Operator expected
			fail();
		} catch(PrologParsingException e){
		}
	}
	
}
