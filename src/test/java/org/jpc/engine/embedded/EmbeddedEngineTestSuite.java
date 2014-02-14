package org.jpc.engine.embedded;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Tests applicable to any embedded Prolog engine.
 * @author sergioc
 *
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
	ObjectReferenceTypesTest.class
})
public class EmbeddedEngineTestSuite {}
