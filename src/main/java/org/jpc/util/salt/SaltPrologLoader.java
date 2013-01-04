package org.jpc.util.salt;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.PrologWriter;
import org.jpc.util.PrologLoader;

public abstract class SaltPrologLoader extends PrologLoader {

	protected PrologWriter writer;
	
	public SaltPrologLoader(PrologEngine logicEngine) {
		super(logicEngine);
		writer = new PrologWriter(logicEngine);
	}

}
