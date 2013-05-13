package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;



public interface PrologEngineManager<T extends PrologEngine> extends PrologEngineFactory<T> {

	public void shutdownPrologEngine(T prologEngine);
	
}
