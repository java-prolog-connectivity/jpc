package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

/**
 * Will instantiate a Prolog engine the first time it is requested and will answer the same engine in any subsequent requests
 * @author sergioc
 *
 */
public class LazyEngineProvider<T extends PrologEngine> implements PrologEngineProvider<T> {

	private PrologEngineFactory<T> prologEngineFactory;
	private T prologEngine;
	
	public LazyEngineProvider(PrologEngineFactory<T> prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
	}
	
	public T getPrologEngine() {
		if(prologEngine == null)
			prologEngine = prologEngineFactory.createPrologEngine();
		return prologEngine;
	}
	
}
