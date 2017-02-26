package org.jpc.util.config;

import static org.jcategory.category.Key.key;
import static org.jpc.util.JpcPreferences.CONFIGURATION_FILE;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jcategory.JCategory;
import org.jcategory.category.CategoryProperty;
import org.jcategory.category.Key;
import org.jpc.JpcException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.provider.LazyEngineProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;

public class EngineConfigurationManager {
	
	private static final Logger logger = LoggerFactory.getLogger(EngineConfigurationManager.class);
	
	private static final Key PROVIDER_KEY = key(LazyEngineProvider.class);
	
	private static EngineConfigurationManager engineConfigurationManager;
	
	public static synchronized EngineConfigurationManager getDefault() {
		if (engineConfigurationManager == null) {
			engineConfigurationManager = createFromFile();
		}
		return engineConfigurationManager;
	}
	
	public static synchronized void setDefault(EngineConfigurationManager engineConfigurationManager) {
		EngineConfigurationManager.engineConfigurationManager = engineConfigurationManager;
	}
	
	/**
	 * Shutdown all the engines registered in the default configuration manager on JVM shutdown.
	 */
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
	
	private final JCategory categorization; //manages a name categorization for providers.
	private final Map<Object,LazyEngineProvider<?>> map; //a map of engine names to providers (not all engine configurations declare a name).
	private final Set<LazyEngineProvider<?>> allProviders; //a separate set is maintained to efficiently find all the providers.
	
	private EngineConfigurationManager() {
		categorization = new JCategory();
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
			if(engineConfiguration.getId() != null) {
				logger.trace("Registering Prolog engine with id: " + engineConfiguration.getId());
				if(map.put(engineConfiguration.getId(), provider) != null)
					throw new JpcException("An engine configuration with id " + engineConfiguration.getId() + " has already been registered.");
			}
			for(String categoryName : engineConfiguration.getCategoryNames()) {
				logger.trace("Registering Prolog engine for category named: " + categoryName);
				if (categorization.forName(categoryName).getLocalProperty(PROVIDER_KEY).isEmpty())
					categorization.forName(categoryName).setProperty(PROVIDER_KEY, provider);
				else
					throw new JpcException("An engine configuration has already been registered for the category named " + categoryName + ".");
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
	
	public <T extends PrologEngine> T getPrologEngineById(Object id) {
		LazyEngineProvider<T> provider = (LazyEngineProvider<T>) map.get(id);
		if(provider != null)
			return provider.getPrologEngine();
		else
			throw new JpcException("No engine with id: " + id + ".");
	}
	
	public <T extends PrologEngine> T getPrologEngine(String categoryName) {
		CategoryProperty<LazyEngineProvider<T>> providerProperty = categorization.forName(categoryName).getProperty(PROVIDER_KEY);
		if(providerProperty.isPresent())
			return providerProperty.get().getPrologEngine();
		else
			throw new JpcException("No engine associated with category name: " + categoryName + ".");
	}
	
	public <T extends PrologEngine> T defaultPrologEngine() {
		return getPrologEngine("");
	}
	
	public PrologEngine getPrologEngine(Package pakkage) {
		return getPrologEngine(pakkage.getName());
	}
	
	public PrologEngine getPrologEngine(Class<?> clazz) {
		return getPrologEngine(getClassCategoryName(clazz));
	}

}
