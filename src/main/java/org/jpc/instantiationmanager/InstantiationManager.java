package org.jpc.instantiationmanager;

import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.minitoolbox.pattern.Factory;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;

/*
 * Utility class mapping abstract classes and interfaces to factories
 */
public class InstantiationManager {
	List<Entry<Class, Factory>> factories;
	
	private static InstantiationManager instantiationManager = new DefaultInstantiationManager();
	
	public static InstantiationManager getDefault() {
		return instantiationManager;
	}
	
	public InstantiationManager() {
		factories = new ArrayList<>();
	}

	protected <CT> Factory<CT> createFactory(Class<CT> clazz) {
		return createFactory(clazz, clazz);
	}
	
	protected <CT> Factory<CT> createFactory(Class<CT> clazz, final Class<? extends CT> instantiationClass) {
		Factory<CT> factory = new Factory<CT>() {
			@Override
			public CT create() {
				try {
					return instantiationClass.newInstance();
				} catch (InstantiationException | IllegalAccessException e) {
					throw new RuntimeException(e);
				}
			}
		};
		return factory;
	}
	

	public void register(Class clazz) {
		register(clazz, clazz);
	}
	
	public <CT> void register(Class<CT> clazz, final Class<? extends CT> instantiationClass) {
		Factory<CT> factory = createFactory(clazz, instantiationClass);
		register(clazz, factory);
	}
	
	public <CT> void register(Class<CT> clazz, Factory<CT> factory) {
		factories.add(0, new AbstractMap.SimpleEntry<Class, Factory>(clazz, factory));
	}
	

	
	
	
	public <T> T instantiate(Type type) {
		Class rawClass = TypeWrapper.wrap(type).getRawClass();
		T instantiation = null;
		try {
			instantiation = (T) rawClass.newInstance();
		} catch (InstantiationException | IllegalAccessException e1) {
		}
		if(instantiation == null) {
			for(Entry<Class, Factory> entry : factories) {
				if(rawClass.isAssignableFrom(entry.getKey())) {
					instantiation = (T) entry.getValue().create();
					break;
				}
			}
		} 
		if(instantiation == null)
			throw new RuntimeException(new InstantiationException("Impossible to instantiate type: " + type));
		return instantiation;
	}

}
