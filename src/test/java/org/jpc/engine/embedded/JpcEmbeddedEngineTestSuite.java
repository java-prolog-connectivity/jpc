package org.jpc.engine.embedded;

import static org.jpc.engine.provider.PrologEngineProviderManager.setPrologEngineProvider;

import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.provider.SimpleEngineProvider;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Jpc embedded engine tests.
 * @author sergioc
 *
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
	EmbeddedEngineTestSuite.class, //tests for any embedded Prolog engine.
	JpcEmbeddedEngineSpecificTest.class //tests specific to the Jpc embedded Prolog engine.
})
public class JpcEmbeddedEngineTestSuite {
	@BeforeClass
	public static void setUp() {
		setPrologEngineProvider(new SimpleEngineProvider(new JpcEngine()));
	}
}
