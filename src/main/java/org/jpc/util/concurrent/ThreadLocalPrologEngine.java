package org.jpc.util.concurrent;

import org.jpc.engine.prolog.PrologEngine;

/**
 * A utility class for configuring one logic engine per thread
 * @author sergioc
 *
 */
public class ThreadLocalPrologEngine {
	
	private static ThreadLocal<PrologEngine> threadLocal = new ThreadLocal<>();

	public static PrologEngine getPrologEngine() {
		return threadLocal.get();
	}
	
	public static void setPrologEngine(PrologEngine prologEngine) {
		threadLocal.set(prologEngine);
	}

}
