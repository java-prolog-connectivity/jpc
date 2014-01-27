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
	
	private static final EngineConfigurationManager engineConfigurationManager = createFromPropertiesFile();
	
	public static EngineConfigurationManager getDefault() {
		return engineConfigurationManager;
	}
	
	private static EngineConfigurationManager createFromPropertiesFile() {
		EngineConfigurationManager manager = new EngineConfigurationManager();
		manager.configureFromFile();
		return manager;
	}
	
	static {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				engineConfigurationManager.shutdownAll(true);
			}
		});
	}
	
	private final JGum jgum; //manages a name categorization for providers.
	private final Map<Object,LazyEngineProvider<?>> map; //a map of alias to providers (not all engine configurations declare an alias).
	private final Set<LazyEngineProvider<?>> allProviders; //a separate set is maintained to efficiently find all the providers.
	
	private EngineConfigurationManager() {
		jgum = new JGum();
		map = new HashMap<>();
		allProviders = new HashSet<>();
	}

	private void configureFromFile() {
		String content = null;
		try(InputStream stream = getClass().getClassLoader().getResourceAsStream(CONFIGURATION_FILE)) {
			if(stream != null)
				content = CharStreams.toString(new InputStreamReader(stream, Charsets.UTF_8));
			else
				logger.trace("Configuration file " + CONFIGURATION_FILE + " not found.");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		if(content != null) {
			JpcConfiguration jpcConfiguration = JpcConfigurationDeserializer.fromJson(content);
			configure(jpcConfiguration);
		}
	}

	private void configure(JpcConfiguration jpcConfiguration) {
		for(EngineConfiguration<?> engineConfiguration : jpcConfiguration.getEngineConfigurations()) {
			LazyEngineProvider<?> provider = new LazyEngineProvider<>(engineConfiguration.getEngineFactory());
			allProviders.add(provider);
			if(engineConfiguration.getAlias() != null) {
				logger.trace("Registering Prolog engine for alias: " + engineConfiguration.getAlias());
				if(map.put(engineConfiguration.getAlias(), provider) != null)
					throw new JpcException("An engine configuration with alias " + engineConfiguration.getAlias() + " has already been registered.");
			}
			for(String packageName : engineConfiguration.getPackageNames()) {
				logger.trace("Registering Prolog engine for package: " + packageName);
				if(!jgum.forName(packageName).getLocalProperty(PROVIDER_KEY).isPresent())
					jgum.forName(packageName).setProperty(PROVIDER_KEY, provider);
				else
					throw new JpcException("An engine configuration has already been registered for package " + packageName + ".");
			}
		}
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
	
	public PrologEngine forAlias(Object alias) {
		return map.get(alias).getPrologEngine();
	}
	
	public <T extends PrologEngine> T forCategoryName(String categoryName) {
		LazyEngineProvider<T> provider = jgum.forName(categoryName).<LazyEngineProvider<T>>getProperty(LazyEngineProvider.class).get();
		return provider.getPrologEngine();
	}
	
	public PrologEngine forPackage(Package pakkage) {
		return forCategoryName(pakkage.getName());
	}
	
	public PrologEngine forClass(Class<?> clazz) {
		return forCategoryName(getClassCategoryName(clazz));
	}

}
