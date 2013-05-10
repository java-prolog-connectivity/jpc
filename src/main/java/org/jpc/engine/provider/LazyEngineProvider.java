package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

/**
 * Will instantiate a Prolog engine the first time it is requested and will answer the same engine in any subsequent requests
 * @author sergioc
 *
 */
public class LazyEngineProvider implements PrologEngineProvider {

	private PrologEngineFactory prologEngineFactory;
	private PrologEngine prologEngine;
	
	public LazyEngineProvider(PrologEngineFactory prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
	}
	
	public synchronized PrologEngine getPrologEngine() {
		if(prologEngine == null)
			prologEngine = prologEngineFactory.createPrologEngine();
		return prologEngine;
	}
	
}
