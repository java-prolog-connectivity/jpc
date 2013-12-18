package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

/**
 * TODO move to interface PrologEngineProvider when Java8 is available
 * Currently used for unit testing.
 * @author sergioc
 *
 */
public class PrologEngineProviderManager {

	private static PrologEngineProvider<? extends PrologEngine> prologEngineProvider;
	
	public static synchronized PrologEngine getPrologEngine() {
		return prologEngineProvider.getPrologEngine();
	}
	
	public static synchronized void setPrologEngineProvider(PrologEngineProvider<? extends PrologEngine> prologEngineProvider) {
		PrologEngineProviderManager.prologEngineProvider = prologEngineProvider;
	}
}
