package org.jpc.engine.prolog;

import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.jgum.JGum;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.engine.provider.LazyEngineProvider;
import org.jpc.util.config.JpcPropertiesFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PrologEngineManager {
	
	private static Logger logger = LoggerFactory.getLogger(PrologEngineManager.class);
	
	private static final Object PROVIDER_KEY = LazyEngineProvider.class;
	
	private static final PrologEngineManager prologEngineManager = createFromPropertiesFile();
	
	public static PrologEngineManager getDefault() {
		return prologEngineManager;
	}
	
	private static PrologEngineManager createFromPropertiesFile() {
		PrologEngineManager manager = new PrologEngineManager();
		manager.configureFromPropertiesFile();
		return manager;
	}
	
	static {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				prologEngineManager.shutdownAll(true);
			}
		});
	}
	
	
	private JGum jgum;
	
	private PrologEngineManager() {
		this.jgum = new JGum();
	}

	private void configureFromPropertiesFile() {
		JpcPropertiesFile jpcProperties;
		try {
			jpcProperties = new JpcPropertiesFile();
			Map<String, Class<PrologEngineFactory<?>>> factoryClassesMap = jpcProperties.getFactoryClassesMap();
			for(Entry<String, Class<PrologEngineFactory<?>>> entry : factoryClassesMap.entrySet()) {
				PrologEngineFactory<?> factory;
				try {
					factory = entry.getValue().newInstance();
				} catch (InstantiationException | IllegalAccessException e) {
					throw new RuntimeException(e);
				}
				LazyEngineProvider<?> provider = new LazyEngineProvider(factory);
				String categoryName = entry.getKey();
				jgum.forName(categoryName).setProperty(PROVIDER_KEY, provider);
			}
		} catch(FileNotFoundException e) {
			logger.trace("Properties file " + JpcPropertiesFile.PREFERENCES_FILE + " not found.");
		}
	}
	
	public void shutdownAll() {
		shutdownAll(false);
	}
	
	public synchronized void shutdownAll(boolean force) {
		for(PrologEngine prologEngine : getAllPrologEngines()) {
			if(prologEngine.isCloseable()) {
				try {
					prologEngine.close();
				} catch(Exception e) {
					logger.warn("Exception when attempting to shutdown prolog engine: " + prologEngine.toString() + ".");
					if(!force)
						throw e;
				}
			}
		}
	}
	
	public synchronized <T extends PrologEngine> Set<T> getAllPrologEngines() {
		List<LazyEngineProvider<T>> providers = jgum.forNameRoot().<LazyEngineProvider<T>>topDownProperties(PROVIDER_KEY);
		Set<T> prologEngines = new HashSet<>();
		for(LazyEngineProvider<T> provider : providers) {
			if(provider.isInitialized()) {
				prologEngines.add(provider.getPrologEngine());
			}	
		}
		return prologEngines;
	}
	
	
	private static String getClassCategoryName(Class<?> clazz) {
		String name = clazz.getCanonicalName();
		if(name == null)
			return getClassCategoryName(clazz.getEnclosingClass());
		else
			return name;
	}
	
	//TODO move the getPrologEngine(...) methods to the interface PrologEngineFactory when Java8 is available.
	public synchronized <T extends PrologEngine> T getPrologEngine(String categoryName) {
		LazyEngineProvider<T> provider = jgum.forName(categoryName).<LazyEngineProvider<T>>getProperty(LazyEngineProvider.class).get();
		return provider.getPrologEngine();
	}
	
	public synchronized PrologEngine getPrologEngine(Package pakkage) {
		return getPrologEngine(pakkage.getName());
	}
	
	public synchronized PrologEngine getPrologEngine(Class<?> clazz) {
		return getPrologEngine(getClassCategoryName(clazz));
	}
	
	
}
