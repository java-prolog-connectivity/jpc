package org.jpc.term.refterm;

import static java.util.Arrays.asList;

import java.util.Map;

import org.jpc.term.Compound;
import org.jpc.term.Integer;

import com.google.common.collect.MapMaker;

/**
 * Manages identifiers for object references and warranties that such identifiers are unique per object.
 * @author sergioc
 *
 */
public class RefTermIdManager {
	
	private static RefTermIdManager defaultJRefTermIdManager = new RefTermIdManager();
	
	public static RefTermIdManager getDefault() {
		return defaultJRefTermIdManager;
	}
	
	private int counter;
	
	private final Map<Object, Compound> currentRefs = new MapMaker().weakKeys().makeMap();
	
	private RefTermIdManager(){}
	
	/**
	 * 
	 * @param ref the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public synchronized Compound jRefTermId(Object ref) {
		return currentRefs.get(ref);
	}

	public synchronized Compound newJRefTermId(Object ref) {
		Compound id = jRefTermId(ref);
		if(id == null) {
			id = new Compound(RefTermManager.JREF_TERM_FUNCTOR_NAME, asList(new Integer(counter++)));
			currentRefs.put(ref, id);
		}
		return id;	
	}
	
}
