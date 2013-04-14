package org.jpc.engine.provider;

import org.jpc.engine.prolog.driver.PrologEngineFactory;

public interface PrologEngineFactoryProvider<T extends PrologEngineFactory> {
	
	public T getPrologEngineFactory();
	
}
