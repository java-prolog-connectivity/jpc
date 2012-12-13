package org.jpc.util;

import org.jpc.engine.prolog.PrologEngine;

/**
 * An abstract class for classes loading a logic theory in a Prolog engine
 * @author sergioc
 *
 */
public abstract class PrologLoader {

	private PrologEngine logicEngine;
	
	public PrologLoader(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}
	
	public PrologEngine getLogicEngine() {
		return logicEngine;
	}
	
	public abstract void load();
}
