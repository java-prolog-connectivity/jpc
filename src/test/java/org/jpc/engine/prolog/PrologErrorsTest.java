package org.jpc.engine.prolog;

import static junit.framework.Assert.assertEquals;
import static org.jpc.engine.provider.PrologEngineProviderManager.getPrologEngine;
import static org.junit.Assert.fail;

import org.jpc.error.PrologParsingException;
import org.jpc.term.Atom;
import org.junit.Test;

public class PrologErrorsTest {

	@Test
	public void testPrologError() {
		try {
			getPrologEngine().query("throw(dummy_error)").hasSolution(); //Arguments are not sufficiently instantiated
			fail();
		} catch(org.jpc.error.PrologError e){
			assertEquals(new Atom("dummy_error"), e.asTerm());
		}
	}
	
	@Test
	public void testInstantiationError() {
		try {
			getPrologEngine().query("A is A").hasSolution(); //Arguments are not sufficiently instantiated
			fail();
		} catch(org.jpc.error.InstantiationError e){
		}
	}

	@Test
	public void testSyntaxError() {
		try {
			getPrologEngine().query("x++y").hasSolution(); //Operator expected
			fail();
		} catch(PrologParsingException e){
		}
	}
	
}
