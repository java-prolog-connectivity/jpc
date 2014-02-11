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
public class JTermIdManager {
	
	private static JTermIdManager defaultJTermIdManager = new JTermIdManager();
	
	public static JTermIdManager getDefault() {
		return defaultJTermIdManager;
	}
	
	private int counter;
	
	private final Map<Object, Compound> currentRefs = new MapMaker().weakKeys().makeMap();
	
	private JTermIdManager(){}
	
	/**
	 * 
	 * @param ref the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public synchronized Compound jTermId(Object ref) {
		return currentRefs.get(ref);
	}

	public synchronized Compound newJTermId(Object ref) {
		Compound id = jTermId(ref);
		if(id == null) {
			id = new Compound(JTermManager.JTERM_FUNCTOR_NAME, asList(new IntegerTerm(counter++)));
			currentRefs.put(ref, id);
		}
		return id;	
	}
	
}
