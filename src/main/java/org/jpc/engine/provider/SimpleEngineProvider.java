package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

/**
 * Currently used for unit testing together with the PrologEngineProviderManager class.
 * @author sergioc
 *
 */
public class SimpleEngineProvider implements PrologEngineProvider {

	private PrologEngine prologEngine;
	
	public SimpleEngineProvider(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}
	
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}
	
}
