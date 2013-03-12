package org.jpc.util.concurrent;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.configuration.PrologEngineConfiguration;

public class ThreadConnectionProvider {

	private PrologEngineConfiguration prologEngineConfiguration;
	
	public ThreadConnectionProvider(PrologEngineConfiguration prologEngineConfiguration) {
		this.prologEngineConfiguration = prologEngineConfiguration;
	}
	
	public PrologEngine getPrologEngine() {
    	PrologEngine prologEngine = ThreadLocalPrologEngine.getPrologEngine();
    	if(prologEngine == null) {
    		//prologEngine = prologEngineConfiguration.getEngine(); //TODO fix it
    		ThreadLocalPrologEngine.setPrologEngine(prologEngine);
    	}
    	return prologEngine;
	}

}
