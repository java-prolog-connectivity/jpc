package org.jpc.util.naming;

import java.util.HashMap;
import java.util.Map;

public abstract class NamingUtil {

	public static void renameRepeatedNames(Iterable<? extends Nameable> nameables) {
		renameRepeatedNames(nameables, new HashMap<String, Integer>());
	}
	
	public static void renameRepeatedNames(Iterable<? extends Nameable> nameables, Map<String, Integer> nameOccurrences) {
		assignUniqueNames(nameables, new DummyNameSolver(), nameOccurrences);
	}
	
	public static void renameIfRepeated(Nameable nameable, Map<String, Integer> nameOccurrences) {
		assignUniqueName(nameable, new DummyNameSolver(), nameOccurrences);
	}
	
	public static void assignUniqueNames(Iterable<? extends Nameable> nameables, NameSolver nameSolver) {
		assignUniqueNames(nameables, nameSolver, new HashMap<String, Integer>());
	}

	public static void assignUniqueNames(Iterable<? extends Nameable> nameables, NameSolver nameSolver, Map<String, Integer> nameOccurrences) {
		for(Nameable nameable : nameables) {
			assignUniqueName(nameable, nameSolver, new HashMap<String, Integer>());
		}
	}
	
	private static void assignUniqueName(Nameable nameable, NameSolver nameSolver, Map<String, Integer> nameOccurrences) {
		String name = nameSolver.nameOf(nameable);
		name = getUniqueName(name, nameOccurrences);
		nameable.setName(name);
	}
	
	public synchronized static String getUniqueName(String name, Map<String, Integer> nameOccurrences) {
		Integer occurrence = nameOccurrences.get(name);
		if(occurrence != null) {
			int next = occurrence+1;
			nameOccurrences.put(name, next);
			name = name + next;
		}else {
			nameOccurrences.put(name, 1);
		}
		return name;
	}

}
