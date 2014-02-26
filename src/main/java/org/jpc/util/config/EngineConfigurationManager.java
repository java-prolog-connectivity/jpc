package org.jpc.util.config;

import static org.jpc.util.JpcPreferences.CONFIGURATION_FILE;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jgum.JGum;
import org.jgum.category.CategoryProperty;
import org.jpc.JpcException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.provider.LazyEngineProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;

public class EngineConfigurationManager {
	
	private static Logger logger = LoggerFactory.getLogger(EngineConfigurationManager.class);
	
	private static final Object PROVIDER_KEY = LazyEngineProvider.class;
	
	private static EngineConfigurationManager engineConfigurationManager;
	
	public static synchronized EngineConfigurationManager getDefault() {
		if(engineConfigurationManager == null)
			engineConfigurationManager = createFromFile();
		return engineConfigurationManager;
	}
	
	public static synchronized void setDefault(EngineConfigurationManager engineConfigurationManager) {
		EngineConfigurationManager.engineConfigurationManager = engineConfigurationManager;
	}
	
	static {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				if(engineConfigurationManager != null)
					engineConfigurationManager.shutdownAll(true);
			}
		});
	}
	
	/**
	 * @return a configuration manager configured according to the default configuration file.
	 */
	public static EngineConfigurationManager createFromFile() {
		return new EngineConfigurationManager().configureFromFile();
	}
	
	/**
	 * @param resource a (class path) filename.
	 * @return a configuration manager configured according to the configuration file passed as parameter.
	 */
	public static EngineConfigurationManager createFromFile(String resource) {
		return new EngineConfigurationManager().configureFromFile(resource);
	}
	
	private final JGum jgum; //manages a name categorization for providers.
	private final Map<Object,LazyEngineProvider<?>> map; //a map of engine names to providers (not all engine configurations declare a name).
	private final Set<LazyEngineProvider<?>> allProviders; //a separate set is maintained to efficiently find all the providers.
	
	private EngineConfigurationManager() {
		jgum = new JGum();
		map = new HashMap<>();
		allProviders = new HashSet<>();
	}

	/**
	 * Configures the configuration manager according to the default configuration file.
	 * @return the configured configuration manager.
	 */
	private EngineConfigurationManager configureFromFile() {
		return configureFromFile(CONFIGURATION_FILE);
	}
	
	/**
	 * Configures the configuration manager according to the configuration file passed as parameter.
	 * @param resource a (class path) filename.
	 * @return the configured configuration manager.
	 */
	private EngineConfigurationManager configureFromFile(String resource) {
		String content = null;
		try(InputStream stream = getClass().getClassLoader().getResourceAsStream(resource)) {
			if(stream != null)
				content = CharStreams.toString(new InputStreamReader(stream, Charsets.UTF_8));
			else
				logger.trace("Configuration file " + resource + " not found.");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		if(content != null) {
			JpcConfiguration jpcConfiguration = JpcConfigurationDeserializer.fromJson(content);
			configure(jpcConfiguration);
		}
		return this;
	}

	private EngineConfigurationManager configure(JpcConfiguration jpcConfiguration) {
		for(EngineConfiguration<?> engineConfiguration : jpcConfiguration.getEngineConfigurations()) {
			LazyEngineProvider<?> provider = new LazyEngineProvider<>(engineConfiguration.getEngineFactory());
			allProviders.add(provider);
			if(engineConfiguration.getName() != null) {
				logger.trace("Registering Prolog engine with name: " + engineConfiguration.getName());
				if(map.put(engineConfiguration.getName(), provider) != null)
					throw new JpcException("An engine configuration with name " + engineConfiguration.getName() + " has already been registered.");
			}
			for(String packageName : engineConfiguration.getPackageNames()) {
				logger.trace("Registering Prolog engine for package: " + packageName);
				if(!jgum.forName(packageName).getLocalProperty(PROVIDER_KEY).isPresent())
					jgum.forName(packageName).setProperty(PROVIDER_KEY, provider);
				else
					throw new JpcException("An engine configuration has already been registered for package " + packageName + ".");
			}
		}
		return this;
	}
	
	public void shutdownAll() {
		shutdownAll(false);
	}
	
	public void shutdownAll(boolean force) {
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
	
	public <T extends PrologEngine> Set<T> getAllPrologEngines() {
		Set<T> prologEngines = new HashSet<>();
		for(LazyEngineProvider<?> provider : allProviders) {
			if(provider.isInitialized()) {
				prologEngines.add((T) provider.getPrologEngine());
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
	
	public <T extends PrologEngine> T getNamedPrologEngine(Object name) {
		LazyEngineProvider<T> provider = (LazyEngineProvider<T>) map.get(name);
		if(provider != null)
			return provider.getPrologEngine();
		else
			throw new JpcException("No engine with name: " + name + ".");
	}
	
	public <T extends PrologEngine> T getPrologEngine(String categoryName) {
		CategoryProperty<LazyEngineProvider<T>> providerProperty = jgum.forName(categoryName).<LazyEngineProvider<T>>getProperty(LazyEngineProvider.class);
		if(providerProperty.isPresent())
			return providerProperty.get().getPrologEngine();
		else
			throw new JpcException("No engine associated with category name: " + categoryName + ".");
	}
	
	public PrologEngine getPrologEngine(Package pakkage) {
		return getPrologEngine(pakkage.getName());
	}
	
	public PrologEngine getPrologEngine(Class<?> clazz) {
		return getPrologEngine(getClassCategoryName(clazz));
	}

}
