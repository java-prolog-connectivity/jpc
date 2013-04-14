package org.jpc.util.naming;

import java.util.HashMap;
import java.util.Map;

public abstract class NamingUtil {

	public static void renameRepeatedNames(Iterable<? extends Nameable> nameables) {
		assignUniqueNames(nameables, new DummyNameSolver());
	}
	
	public static void renameIfRepeated(Nameable nameable, Map<String, Integer> nameOccurrences) {
		assignUniqueName(nameable, new DummyNameSolver(), nameOccurrences);
	}
	
	public static void assignUniqueNames(Iterable<? extends Nameable> nameables, NameSolver nameSolver) {
		Map<String, Integer> nameOccurrences = new HashMap<String, Integer>();
		for(Nameable nameable : nameables) {
			assignUniqueName(nameable, nameSolver, new HashMap<String, Integer>());
		}
	}
	
	private static void assignUniqueName(Nameable nameable, NameSolver nameSolver, Map<String, Integer> nameOccurrences) {
		String name = nameSolver.nameOf(nameable);
		Integer occurrence = nameOccurrences.get(name);
		if(occurrence != null) {
			int next = occurrence+1;
			nameOccurrences.put(name, next);
			name = name + next;
		} else {
			nameOccurrences.put(name, 1);
		}
		nameable.setName(name);
	}

}
