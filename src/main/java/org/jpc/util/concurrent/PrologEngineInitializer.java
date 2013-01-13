package org.jpc.util.concurrent;

import java.util.concurrent.Callable;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineConfiguration;

public class PrologEngineInitializer implements Callable<PrologEngine> {

	private PrologEngineConfiguration prologEngineConfiguration;
	
	public PrologEngineInitializer(PrologEngineConfiguration prologEngineConfiguration) {
		this.prologEngineConfiguration = prologEngineConfiguration;
	}
	
	@Override
	public PrologEngine call() throws Exception {
		return prologEngineConfiguration.getEngine(); //the logic engine is instantiated in the execution context of the thread invoking this callable
	}

}
