package org.jpc.util.concurrent;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.PrologEngineDependant;

public abstract class JpcRunnable implements Runnable, PrologEngineDependant {

	private PrologEngine prologEngine;

	@Override
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}

	@Override
	public void setPrologEngine(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}

}
