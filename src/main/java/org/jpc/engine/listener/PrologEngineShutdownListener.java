package org.jpc.engine.listener;

import org.jpc.engine.prolog.PrologEngine;

public interface PrologEngineShutdownListener {

	void onPrologEngineShutdown(PrologEngine prologEngine);
	
}
