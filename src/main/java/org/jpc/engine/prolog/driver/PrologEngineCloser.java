package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineCloser {

	public void shutdownPrologEngine(PrologEngine prologEngine);
	
}
