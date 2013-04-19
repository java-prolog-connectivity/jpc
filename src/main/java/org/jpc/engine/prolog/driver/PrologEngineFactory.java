package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineFactory<T extends PrologEngine> {

	public T createPrologEngine();

	/**
	 * Answers if the factory can create new Prolog engines
	 * @return
	 */
	public boolean isEnabled();
}
