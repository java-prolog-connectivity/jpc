package org.jpc.engine.prolog.driver;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineFactory {

	public PrologEngine createPrologEngine();

	/**
	 * Answers if the factory cannot create new Prolog engines
	 * @return
	 */
	public boolean isDisabled();

}
