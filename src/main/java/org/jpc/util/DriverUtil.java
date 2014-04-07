package org.jpc.util;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jpc.engine.listener.DriverStateListener;
import org.jpc.engine.prolog.driver.PrologEngineDriver;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.googlereflections.GoogleReflectionsUtil;
import org.reflections.Reflections;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.google.common.collect.Multimap;
import com.google.common.collect.TreeMultimap;

public class DriverUtil {
	
	public static void reportFoundDrivers() {
		Set<PrologEngineDriver> drivers = findDrivers();
		System.out.println("Drivers found: " + drivers.size());
		for(PrologEngineDriver driver : drivers) {
			System.out.println("Driver name: " + driver.getLibraryName() + ". Description: " + driver.getDescription());
		}
	}
	
	//TODO delete this method when lambdas are available in java 8
	public static void registerListener(DriverStateListener listener, Iterable<? extends PrologEngineDriver> drivers) {
		for(PrologEngineDriver driver : drivers) {
			driver.addStateListener(listener);
		}
	}
	
	public static Set<PrologEngineDriver> findDrivers() {
		return findDrivers(new ArrayList<ClassLoader>(), new ArrayList<URL>());
	}
	
	public static Set<PrologEngineDriver> findDrivers(Collection<ClassLoader> classLoaders) {
		return findDrivers(classLoaders, new ArrayList<URL>());
	}
	
	public static Set<PrologEngineDriver> findDrivers(Collection<ClassLoader> classLoaders, Collection<URL> urls) {
		Set<PrologEngineDriver> drivers = new HashSet<>();
		for(Class<? extends PrologEngineDriver> clazz : findDriverClasses(classLoaders, urls)) {
			try {
				PrologEngineDriver driver = clazz.newInstance();
				if(!driver.isDisabled())
					drivers.add(driver);
			} catch (InstantiationException | IllegalAccessException e) {
			} //do nothing if the configuration class cannot be instantiated
		}
		return drivers;
	}
	
//	public static Set<Class<? extends PrologEngineDriver>> findDriverClasses(URL ...urls) {
//		return findDriverClasses(Arrays.asList(urls));
//	}
	
	/**
	 * Answers a set with all the Prolog drivers found in the given urls.
	 * If the urls are empty will answer the drivers in the class loader classpath
	 * @param urls
	 * @return
	 */
	public static Set<Class<? extends PrologEngineDriver>> findDriverClasses(Collection<ClassLoader> classLoaders, Collection<URL> urls) {
		ConfigurationBuilder config = new ConfigurationBuilder();
		if(classLoaders.isEmpty() && urls.isEmpty()) {
			urls = ClasspathHelper.forClassLoader();
			//urls = ClasspathHelper.forPackage(JPC_BASE_PACKAGE);
		}
		Set<URL> fixedUrls = GoogleReflectionsUtil.fixURLs(urls);
		if(!classLoaders.isEmpty())
			config.addClassLoaders(classLoaders);
		if(!urls.isEmpty())
			config.addUrls(fixedUrls);
		Reflections reflections = new Reflections(config);
		Set<Class<? extends PrologEngineDriver>> foundClasses = reflections.getSubTypesOf(PrologEngineDriver.class);
		return ReflectionUtil.filterAbstractClasses(foundClasses);
	}

	
	public static class PrologEngineTypeComparator implements Comparator<String> {
		@Override
		public int compare(String o1, String o2) {
			return o1.compareTo(o2);
		}
	}
	
	public static class PrologEngineDriverComparator implements Comparator<PrologEngineDriver> {
		@Override
		public int compare(PrologEngineDriver o1, PrologEngineDriver o2) {
			return o1.getLibraryName().compareTo(o2.getLibraryName());
		}
	}
	
	/**
	 * Returns a map where its key are Prolog engine names.
	 * Each entry in this map is a multimap mapping bridge library names to prolog drivers.
	 * In this way, a bridge library could be mapped to more than one Prolog driver.
	 * @param configurations
	 * @return
	 */
	public static <T extends PrologEngineDriver> Map<String,Multimap<String, T>> groupByPrologEngineName(Iterable<T> drivers) {
		Map<String,Multimap<String, T>> dictionary = new HashMap<>();
		for(T driver : drivers) {
			Multimap<String, T> matchedLibraryNameMultiMap = dictionary.get(driver.getEngineDescription().getName()); //find existing entries for the configuration engine id
			if(matchedLibraryNameMultiMap == null) {
				matchedLibraryNameMultiMap = TreeMultimap.create(new PrologEngineTypeComparator(), new PrologEngineDriverComparator());
				dictionary.put(driver.getEngineDescription().getName(), matchedLibraryNameMultiMap);
			}
			matchedLibraryNameMultiMap.put(driver.getLibraryName(), driver);
		}
		return dictionary;
	}

	/**
	 * Returns a map where its key are bridge library names.
	 * Each entry in this map is a multimap mapping Prolog engine names to prolog configurations.
	 * @param configurations
	 * @return
	 */
	public static <T extends PrologEngineDriver> Map<String,Multimap<String, T>> groupByLibraryName(Iterable<T> drivers) {
		Map<String,Multimap<String, T>> dictionary = new HashMap<>();
		for(T driver : drivers) {
			Multimap<String, T> matchedEngineNameMultiMap = dictionary.get(driver.getLibraryName()); //find existing entries for the configuration library id
			if(matchedEngineNameMultiMap == null) {
				matchedEngineNameMultiMap = TreeMultimap.create(new PrologEngineTypeComparator(), new PrologEngineDriverComparator());
				dictionary.put(driver.getLibraryName(), matchedEngineNameMultiMap);
			}
			matchedEngineNameMultiMap.put(driver.getEngineDescription().getName(), driver);
		}
		return dictionary;
	}

	/**
	 * Order alphabetically a list of drivers.
	 * @param drivers
	 */
	public static void orderByLibraryName(List<? extends PrologEngineDriver> drivers) {
		Collections.sort(drivers, new Comparator<PrologEngineDriver>(){
			@Override
			public int compare(PrologEngineDriver d1, PrologEngineDriver d2) {
				return d1.getLibraryName().compareTo(d2.getLibraryName());
			}
		});
	}

}
