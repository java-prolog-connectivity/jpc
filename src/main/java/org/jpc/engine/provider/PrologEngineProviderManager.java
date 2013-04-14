package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

/**
 * TODO find a way to eliminate this singleton (probably could be in a jpc context)
 * @author sergioc
 *
 */
public class PrologEngineProviderManager {

	private volatile static PrologEngineProvider prologEngineProvider;
	
	public static PrologEngine getPrologEngine() {
		return prologEngineProvider.getPrologEngine();
	}
	
	public static void setPrologEngineProvider(PrologEngineProvider prologEngineProvider) {
		PrologEngineProviderManager.prologEngineProvider = prologEngineProvider;
	}
}
