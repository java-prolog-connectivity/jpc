package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

/**
 * Currently used for unit testing.
 * @author sergioc
 *
 */
public class PrologEngineProviderManager {

	private static PrologEngineProvider prologEngineProvider;
	
	public static synchronized PrologEngine getPrologEngine() {
		return prologEngineProvider.getPrologEngine();
	}
	
	public static synchronized void setPrologEngineProvider(PrologEngineProvider prologEngineProvider) {
		PrologEngineProviderManager.prologEngineProvider = prologEngineProvider;
	}
}
