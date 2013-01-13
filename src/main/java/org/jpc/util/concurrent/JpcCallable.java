package org.jpc.util.concurrent;

import java.util.concurrent.Callable;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.PrologEngineDependant;

public abstract class JpcCallable<T> implements Callable<T>, PrologEngineDependant {

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
