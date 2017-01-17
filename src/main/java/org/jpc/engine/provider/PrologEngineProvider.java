package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineProvider<T extends PrologEngine> {

	T getPrologEngine();
	
}
