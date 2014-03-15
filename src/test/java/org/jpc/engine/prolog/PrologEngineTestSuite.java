package org.jpc.engine.prolog;

import org.jpc.util.engine.PrologResourceLoaderTest;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * A collection of unit tests requirying a concrete Prolog engine.
 * Any library serving as a bridge from JPC to itself should include these tests.
 * The tests in this suite are limited to evaluating the minimum required Prolog functionality (does not test Logtalk support).
 * @author sergioc
 *
 */
@RunWith(Suite.class)
@SuiteClasses({
	PrologEngineTest.class, 
	SymbolicReferenceTypesTest.class,
	AnsweringJavaObjectsTest.class,
	PrologErrorsTest.class, 
	QueryLifeCycleTest.class, 
	ObservableQueryTest.class, 
	PrologResourceLoaderTest.class
})
public class PrologEngineTestSuite {}
