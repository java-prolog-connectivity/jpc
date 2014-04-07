package org.jpc.term.jrefterm;

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
public class JRefTermIdManager {
	
	private static JRefTermIdManager defaultJRefTermIdManager = new JRefTermIdManager();
	
	public static JRefTermIdManager getDefault() {
		return defaultJRefTermIdManager;
	}
	
	private int counter;
	
	private final Map<Object, Compound> currentRefs = new MapMaker().weakKeys().makeMap();
	
	private JRefTermIdManager(){}
	
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
			id = new Compound(JRefTermManager.JREF_TERM_FUNCTOR_NAME, asList(new IntegerTerm(counter++)));
			currentRefs.put(ref, id);
		}
		return id;	
	}
	
}