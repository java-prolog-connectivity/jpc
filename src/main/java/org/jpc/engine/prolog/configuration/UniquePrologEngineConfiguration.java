package org.jpc.engine.prolog.configuration;

import org.jpc.engine.prolog.PrologEngine;

/**
 * This class describes logic engines that are unique per process
 * Instances can cache the logic engine when creating it
 * @author sergioc
 *
 */
public abstract class UniquePrologEngineConfiguration extends PrologEngineConfiguration {
	
	public abstract boolean isInstanceRunning();
	
	@Override
	public synchronized PrologEngine createPrologEngine() {
		if(isInstanceRunning())
			return basicCreatePrologEngine();
		else
			return super.createPrologEngine();
	}

}
