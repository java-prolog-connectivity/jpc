package org.jpc.engine.logtalk;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * This class add Logtalk related tests to the Prolog related tests listed in PrologEngineTestSuite.
 * If a JPC compatible engine supports Logtalk, it should pass the tests in this suite. 
 * If it does not support Logtalk, it should only pass tests declared in PrologEngineTestSuite.
 * @author sergioc
 *
 */
@RunWith(Suite.class)
@SuiteClasses({LogtalkEngineTest.class, LogtalkSideApiTest.class})
public class LogtalkEngineTestSuite {}
