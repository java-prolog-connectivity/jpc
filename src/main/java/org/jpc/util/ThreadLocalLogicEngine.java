package org.jpc.util;

import org.jpc.engine.prolog.PrologEngine;

/**
 * A utility class for configuring one logic engine per thread
 * @author sergioc
 *
 */
public class ThreadLocalLogicEngine {
	
	private static ThreadLocal<PrologEngine> threadLocal = new ThreadLocal();

	public static PrologEngine getLogicEngine() {
		return threadLocal.get();
	}
	
	public static void setLogicEngine(PrologEngine logicEngine) {
		threadLocal.set(logicEngine);
	}

}
