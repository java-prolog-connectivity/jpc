package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;



public interface PrologEngineManager extends PrologEngineFactory {

	public void shutdownPrologEngine(PrologEngine prologEngine);
	
}
