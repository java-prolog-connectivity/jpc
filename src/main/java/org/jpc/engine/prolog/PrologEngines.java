package org.jpc.engine.prolog;

import java.util.Set;

import org.jpc.util.config.EngineConfigurationManager;

public abstract class PrologEngines {

	public static void shutdownAll() {
		EngineConfigurationManager.getDefault().shutdownAll();
	}
	
	public static void shutdownAll(boolean force) {
		EngineConfigurationManager.getDefault().shutdownAll(force);
	}
	
	public static <T extends PrologEngine> Set<T> getAllPrologEngines() {
		return EngineConfigurationManager.getDefault().getAllPrologEngines();
	}
	
	public static <T extends PrologEngine> T getPrologEngineById(Object id) {
		return EngineConfigurationManager.getDefault().getPrologEngineById(id);
	}
	
	public static <T extends PrologEngine> T getPrologEngine(String categoryName) {
		return EngineConfigurationManager.getDefault().getPrologEngine(categoryName);
	}
	
	public static <T extends PrologEngine> T defaultPrologEngine() {
		return EngineConfigurationManager.getDefault().defaultPrologEngine();
	}
	
	public static PrologEngine getPrologEngine(Package pakkage) {
		return EngineConfigurationManager.getDefault().getPrologEngine(pakkage);
	}
	
	public static PrologEngine getPrologEngine(Class<?> clazz) {
		return EngineConfigurationManager.getDefault().getPrologEngine(clazz);
	}

}
