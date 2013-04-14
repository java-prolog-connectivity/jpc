package org.jpc.util.concurrent;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.engine.provider.PrologEngineProvider;

public class ThreadLocalPrologEngineProvider implements PrologEngineProvider {

	private PrologEngineFactory prologEngineFactory;
	private PrologEngine prologEngine;
	
	public ThreadLocalPrologEngineProvider(PrologEngineFactory prologEngineFactory) {
		this.prologEngineFactory = prologEngineFactory;
	}
	
	@Override
	public PrologEngine getPrologEngine() {
    	PrologEngine threadLocalPrologEngine = ThreadLocalPrologEngine.getPrologEngine();
    	if(threadLocalPrologEngine == null) {
    		if(prologEngine == null) {
    			prologEngine = prologEngineFactory.createPrologEngine();
    		}
    		threadLocalPrologEngine = prologEngine;
    		ThreadLocalPrologEngine.setPrologEngine(threadLocalPrologEngine);
    	}
    	return threadLocalPrologEngine;
	}

}
