package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import java.util.Map;

import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

import com.google.common.collect.MapMaker;

/**
 * Manages identifiers for object references and warranties that such identifiers are unique per object.
 * @author sergioc
 *
 */
public class WeakJTermIdManager {
	
	private static WeakJTermIdManager defaultJTermIdManager = new WeakJTermIdManager();
	
	public static WeakJTermIdManager getDefault() {
		return defaultJTermIdManager;
	}
	
	private int counter = 0;
	
	private final Map<Object, Compound> currentRefs = new MapMaker().weakKeys().makeMap();
	
	private WeakJTermIdManager(){}
	
	/**
	 * 
	 * @param ref the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public Compound weakJTerm(Object ref) {
		return currentRefs.get(ref);
	}

	public synchronized Compound newWeakJTerm(Object ref) {
		Compound id = weakJTerm(ref);
		if(id == null) {
			id = new Compound(JTermManager.JTERM_FUNCTOR_NAME, asList(new IntegerTerm(++counter)));
			currentRefs.put(ref, id);
		}
		return id;	
	}
	
}
