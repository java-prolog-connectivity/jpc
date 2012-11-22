package org.jpc.util;

import org.jpc.engine.LogicEngine;

/**
 * A utility class for configuring one logic engine per thread
 * @author sergioc
 *
 */
public class ThreadLocalLogicEngine {
	
	private static ThreadLocal<LogicEngine> threadLocal = new ThreadLocal();

	public static LogicEngine getLogicEngine() {
		return threadLocal.get();
	}
	
	public static void setLogicEngine(LogicEngine logicEngine) {
		threadLocal.set(logicEngine);
	}

}
