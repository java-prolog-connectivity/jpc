package org.jpc.engine.profile;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

public class LogtalkEngineProfile<T extends PrologEngine> extends PrologEngineProfile<T> {

	public LogtalkEngineProfile(PrologEngineFactory<T> engineFactory) {
		super(engineFactory);
	}
	
	@Override
	public void onCreate(T newPrologEngine) {
		newPrologEngine.withLogtalk();
	}
	
}
