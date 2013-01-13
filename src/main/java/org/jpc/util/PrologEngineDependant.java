package org.jpc.util;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineDependant {

	public PrologEngine getPrologEngine();

	public void setPrologEngine(PrologEngine prologEngine);

}
