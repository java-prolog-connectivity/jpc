package org.jpc.util;

import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.driver.PrologEngineDriver;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.googlereflections.GoogleReflectionsUtil;
import org.reflections.Reflections;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;

public class DriverUtil {
	
	//TODO delete this method when lambdas are available in java 8
	public static void registerListener(DriverStateListener listener, Iterable<? extends PrologEngineDriver> drivers) {
		for(PrologEngineDriver driver : drivers) {
			driver.addStateListener(listener);
		}
	}
	
	/**
	 * Answers an ordered list of drivers.
	 * The ordering criteria are the driver engine name, the driver bridge library name and the driver name (in that order).
	 * @param drivers the drivers to order
	 * @return an ordered list of drivers
	 */
	public static List<PrologEngineDriver> order(Iterable<? extends PrologEngineDriver> drivers) {
		List<PrologEngineDriver> orderedList = Lists.newArrayList(drivers);
		Collections.sort(orderedList, new Comparator<PrologEngineDriver>(){
			@Override
			public int compare(PrologEngineDriver d1, PrologEngineDriver d2) {
				if(!d1.getEngineName().equals(d2.getEngineName()))
					return d1.getEngineName().compareTo(d2.getEngineName());
				else if(!d1.getLibraryName().equals(d2.getLibraryName()))
					return d1.getLibraryName().compareTo(d2.getLibraryName());
				else
					return d1.getName().compareTo(d2.getName());
			}});
		return orderedList;
	}
	
	public static Set<PrologEngineDriver> findConfigurations(URL ...urls) {
		return findDrivers(Arrays.asList(urls));
	}
	
	public static Set<PrologEngineDriver> findDrivers(Iterable<URL> urls) {
		Set<PrologEngineDriver> drivers = new HashSet<>();
		for(Class<? extends PrologEngineDriver> clazz : findDriverClasses(urls)) {
			try {
				PrologEngineDriver driver = clazz.newInstance();
				drivers.add(driver);
			} catch (InstantiationException | IllegalAccessException e) {
			} //do nothing if the configuration class cannot be instantiated
		}
		return drivers;
	}
	
	public static Set<Class<? extends PrologEngineDriver>> findDriverClasses(URL ...urls) {
		return findDriverClasses(Arrays.asList(urls));
	}
	
	/**
	 * Answers a set with all the Prolog drivers found in the given urls.
	 * If the urls are empty will answer the drivers in the class loader classpath
	 * @param urls
	 * @return
	 */
	public static Set<Class<? extends PrologEngineDriver>> findDriverClasses(Iterable<URL> urls) {
		ConfigurationBuilder config = new ConfigurationBuilder();
		if(!urls.iterator().hasNext()) {
			urls = ClasspathHelper.forClassLoader();
			//urls = ClasspathHelper.forPackage(JPC_BASE_PACKAGE);
		}
		Set<URL> fixedUrls = GoogleReflectionsUtil.fixURLs(urls);
		config.addUrls(fixedUrls);
		Reflections reflections = new Reflections(config);
		return ReflectionUtil.filterAbstractClasses(reflections.getSubTypesOf(PrologEngineDriver.class));
	}
	
	/**
	 * Returns a multimap where its key are Prolog engine names.
	 * Each entry in this multimap is also a multimap mapping library names to prolog drivers.
	 * @param configurations
	 * @return
	 */
	public static Multimap<String,Multimap<String, PrologEngineDriver>> groupByPrologEngineName(Iterable<? extends PrologEngineDriver> drivers) {
		ArrayListMultimap<String,Multimap<String, PrologEngineDriver>> engineNameMultimap = ArrayListMultimap.create();
		for(PrologEngineDriver driver : drivers) {
			Collection<Multimap<String, PrologEngineDriver>> libraryNameMultiMapList = engineNameMultimap.get(driver.getEngineName()); //find existing entries for the configuration engine name
			Multimap<String, PrologEngineDriver> matchedLibraryNameMultiMap = null;
			for(Multimap<String, PrologEngineDriver> libraryNameMultiMap : libraryNameMultiMapList) {
				if(!libraryNameMultiMap.get(driver.getLibraryName()).isEmpty()) {
					matchedLibraryNameMultiMap = libraryNameMultiMap;
					break;
				}
			}
			if(matchedLibraryNameMultiMap == null) {
				matchedLibraryNameMultiMap = ArrayListMultimap.create();
				engineNameMultimap.put(driver.getEngineName(), matchedLibraryNameMultiMap);
			}
			matchedLibraryNameMultiMap.put(driver.getLibraryName(), driver);
		}
		return engineNameMultimap;
	}

	/**
	 * Returns a multimap where its key are bridge library names.
	 * Each entry in this multimap is also a multimap mapping Prolog engine names to prolog configurations.
	 * @param configurations
	 * @return
	 */
	public static Multimap<String,Multimap<String, PrologEngineDriver>> groupByLibraryName(Iterable<? extends PrologEngineDriver> drivers) {
		ArrayListMultimap<String,Multimap<String, PrologEngineDriver>> libraryNameMultimap = ArrayListMultimap.create();
		for(PrologEngineDriver driver : drivers) {
			Collection<Multimap<String, PrologEngineDriver>> engineNameMultiMapList = libraryNameMultimap.get(driver.getLibraryName()); //find existing entries for the configuration library name
			Multimap<String, PrologEngineDriver> matchedEngineNameMultiMap = null;
			for(Multimap<String, PrologEngineDriver> engineNameMultiMap : engineNameMultiMapList) {
				if(!engineNameMultiMap.get(driver.getEngineName()).isEmpty()) {
					matchedEngineNameMultiMap = engineNameMultiMap;
					break;
				}
			}
			if(matchedEngineNameMultiMap == null) {
				matchedEngineNameMultiMap = ArrayListMultimap.create();
				libraryNameMultimap.put(driver.getLibraryName(), matchedEngineNameMultiMap);
			}
			matchedEngineNameMultiMap.put(driver.getEngineName(), driver);
		}
		return libraryNameMultimap;
	}



	
}
