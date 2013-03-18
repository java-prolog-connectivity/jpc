package org.jpc.converter.instantiation;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;

public class DefaultInstantiationManager extends InstantiationManager {

	public DefaultInstantiationManager() {
		registerDefaultFactories();
	}
	
	private void registerDefaultFactories() {
		register(ArrayDeque.class);
		register(HashMap.class);
		register(HashSet.class);
		register(ArrayList.class);
		register(GregorianCalendar.class);
	}
	
}
