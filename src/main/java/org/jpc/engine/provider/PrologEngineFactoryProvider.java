package org.jpc.engine.provider;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

public interface PrologEngineFactoryProvider<T extends PrologEngine> {
	
	PrologEngineFactory<T> getPrologEngineFactory();
	
}
