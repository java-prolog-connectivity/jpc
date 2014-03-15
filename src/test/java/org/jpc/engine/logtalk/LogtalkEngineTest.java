package org.jpc.engine.logtalk;

import static org.jpc.engine.logtalk.LogtalkConstants.LOGTALK_LOGTALK_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.LOGTALK_OPERATOR;
import static org.jpc.engine.logtalk.LogtalkConstants.USER_LOGTALK_OBJECT;
import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.jpc.term.Atom;
import org.junit.Test;

/**
 * This class tests the minimum required Logtalk functionality
 * (to be completed)
 * @author scastro
 *
 */
public class LogtalkEngineTest {
	
	/**
	 * verify that the Logtalk operators have been defined in the logic engine
	 */
	@Test
	public void testLogtalkOperators() {
		assertTrue(defaultPrologEngine().isBinaryOperator(LOGTALK_OPERATOR));
		assertTrue(defaultPrologEngine().isUnaryOperator(LOGTALK_OPERATOR));
	}

	/**
	 * Verify that the Logtalk object "logtalk" has been defined
	 */
	@Test
	public void testCurrentObjects() {
		assertTrue(new LogtalkEngine(defaultPrologEngine()).currentObject(new Atom(LOGTALK_LOGTALK_OBJECT)).hasSolution());
		assertTrue(new LogtalkEngine(defaultPrologEngine()).currentObject(new Atom(USER_LOGTALK_OBJECT)).hasSolution());
//		List<Term> currentObjectsTerms = new Jpc().toTerm(new LogtalkEngine(getPrologEngine()).currentObjects()).asList();
//		assertTrue(currentObjectsTerms.contains(new Atom(LOGTALK_LOGTALK_OBJECT)));
//		assertTrue(currentObjectsTerms.contains(new Atom(USER_LOGTALK_OBJECT)));
	}

	@Test
	public void testLogtalkLibraries() {
		Map<String, LogtalkLibrary> libraries = new LogtalkEngine(defaultPrologEngine()).getLibraries();
		assertTrue(libraries.size() > 0);
		assertNotNull(libraries.get("library"));
	}
}
