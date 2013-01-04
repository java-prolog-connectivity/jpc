package org.jpc.util;

import org.jpc.engine.prolog.PrologEngine;

/**
 * An abstract class for classes loading a logic theory in a Prolog engine
 * @author sergioc
 *
 */
public abstract class PrologLoader {

	protected PrologEngine logicEngine;
	
	public PrologLoader(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
	}
	
	public abstract void load();
}
