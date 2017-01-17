package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineFactory<T extends PrologEngine> {

	T createPrologEngine();

}
