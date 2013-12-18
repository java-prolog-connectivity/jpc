package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

/**
 * Instantiates a new Prolog engine the first time one is requested and will answer the same cached engine in any subsequent requests.
 * @author sergioc
 *
 */
public class LazyEngineProvider<T extends PrologEngine> implements PrologEngineProvider<T> {

	private PrologEngineFactory<T> prologEngineFactory;
	private T prologEngine;
	
	public LazyEngineProvider(PrologEngineFactory<T> prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
	}
	
	public synchronized T getPrologEngine() {
		if(!isInitialized())
			prologEngine = prologEngineFactory.createPrologEngine();
		return prologEngine;
	}
	
	public synchronized boolean isInitialized() {
		return prologEngine != null;
	}
	
}
