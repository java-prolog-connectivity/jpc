package org.jpc.util;

import static org.jpc.util.JpcPreferences.JPC_BASE_PACKAGE;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineConfiguration;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.googlereflections.GoogleReflectionsUtil;
import org.reflections.Reflections;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

/**
 * This class administers multiple PrologEngineConfigurations in a pool.
 * A PrologEngineConfiguration answers always the same logic engine when its method 'getEngine' is invoked,
 * then this class can be useful if the programmer needs to refer, from different parts of a program, to the same prolog engine given an engine configuration.
 * The programmer needs to be aware that some engine configurations are incompatible (e.g., it is not possible to create a JPL logic engine configured for SWI and another configured for YAP in the same program).
 * Therefore, this class stores in its repository engine configurations, not instance of the engines themselves.
 * The user of the class should decide which of the registered engine configurations can be used to create logic engines.
 * @author sergioc
 *
 */
public class PrologEngineManager {
	
	private static PrologEngineManager defaultPrologEngineManager = new PrologEngineManager();
	
	public static PrologEngineManager getDefault() {
		return defaultPrologEngineManager;
	}
	
	public static Set<Class<? extends PrologEngineConfiguration>> findConfigurations(URL ...urls) {
		return findConfigurations(new HashSet<URL>(Arrays.asList(urls)));
	}
	
	public static Set<Class<? extends PrologEngineConfiguration>> findConfigurations(Set<URL> urls) {
		Set<URL> fixedUrls = GoogleReflectionsUtil.fixURLs(urls);
		ConfigurationBuilder config = new ConfigurationBuilder();
		if(urls.size() > 0)
			config.addUrls(fixedUrls);
		else
			config.addUrls(ClasspathHelper.forPackage(JPC_BASE_PACKAGE));
		Reflections reflections = new Reflections(config);
		return ReflectionUtil.filterAbstractClasses(reflections.getSubTypesOf(PrologEngineConfiguration.class));
	}
	
	private Map<Class<? extends PrologEngineConfiguration>, PrologEngineConfiguration> engineConfigurationPool; // a repository of logic engine configurations
	
	public PrologEngineManager() {
		engineConfigurationPool = new LinkedHashMap<>(); //to preserve insertion order
	}
	
	public List<PrologEngineConfiguration> register(Iterable<Class<? extends PrologEngineConfiguration>> prologEngineConfigurationClasses) {
		List<PrologEngineConfiguration> configs = new ArrayList<>();
		for(Class<? extends PrologEngineConfiguration> prologEngineConfigurationClass : prologEngineConfigurationClasses) {
			configs.add(register(prologEngineConfigurationClass));
		}
		return configs;
	}
	
	/**
	 * Adds a PrologEngineConfiguration class to the pool if an instance is not already there. If an instance is already in the pool, it does nothing and returns the existing instance.
	 * @param prologEngineConfigurationClass the class to register in the pool
	 * @return an existing instance of the class (previously registered in the pool) or a new instance (added to the pool) if the class was not registered before
	 */
	public PrologEngineConfiguration register(Class<? extends PrologEngineConfiguration> prologEngineConfigurationClass) {
		PrologEngineConfiguration prologEngineConfiguration = engineConfigurationPool.get(prologEngineConfigurationClass);
		
		if(prologEngineConfiguration == null) {
			try {
				prologEngineConfiguration = prologEngineConfigurationClass.newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
			engineConfigurationPool.put(prologEngineConfigurationClass, prologEngineConfiguration);
		}
		return prologEngineConfiguration;
	}
	
	/**
	 * Answers a logic engine according to an engine configuration (using an existing instance of the configuration if available)
	 * 
	 * @param prologEngineConfigurationClass
	 * @return
	 */
	public PrologEngine getPrologEngine(Class<? extends PrologEngineConfiguration> prologEngineConfigurationClass) {
		return register(prologEngineConfigurationClass).getEngine();
	}
	
	public List<PrologEngineConfiguration> getAllConfigurations() {
		return new ArrayList<>(engineConfigurationPool.values());
	}
	
	public Multimap<String,PrologEngineConfiguration> groupByPrologEngine() {
		ArrayListMultimap<String, PrologEngineConfiguration> multimap = ArrayListMultimap.create();
		for(PrologEngineConfiguration config : getAllConfigurations()) {
			multimap.put(config.getEngineName(), config);
		}
		return multimap;
	}

	public Multimap<String,PrologEngineConfiguration> groupByLibraryName() {
		ArrayListMultimap<String, PrologEngineConfiguration> multimap = ArrayListMultimap.create();
		for(PrologEngineConfiguration config : getAllConfigurations()) {
			multimap.put(config.getLibraryName(), config);
		}
		return multimap;
	}

}
