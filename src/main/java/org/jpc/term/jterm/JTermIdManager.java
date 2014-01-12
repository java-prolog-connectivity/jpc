package org.jpc.term.jterm;

import java.util.Map;

import com.google.common.collect.MapMaker;

/**
 * Manages identifiers for object references and warranties that such identifiers are unique per object.
 * @author sergioc
 *
 */
public class JTermIdManager {
	
	private static JTermIdManager defaultJTermIdManager = new JTermIdManager();
	
	public static JTermIdManager getJRefIdManager() {
		return defaultJTermIdManager;
	}
	
	private int counter = 0;
	
	/**
	 * This map associates objects with their reference ids.
	 * The map discards an entry if the key (the object associated with a reference id) is marked for garbage collection.
	 * Quoting from the Guava documentation (http://docs.guava-libraries.googlecode.com/git-history/release/javadoc/com/google/common/collect/MapMaker.html) :
	 * 
	 * "Note: by default, the returned map uses equality comparisons (the equals method) to determine equality for keys or values. 
	 * However, if weakKeys() was specified, the map uses identity (==) comparisons instead for keys. 
	 * Likewise, if weakValues() or softValues() was specified, the map uses identity comparisons for values. "
	 * 
	 * Therefore, our map uses identity comparisons. This is a desirable property, since we need different references ids for objects with different references, and this is independent of the equals() method being overridden.
	 */
	private final Map<Object, JTermId> currentRefs = new MapMaker().weakKeys().makeMap();
	
	private JTermIdManager(){}
	
	/**
	 * 
	 * @param o the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public JTermId get(Object o) {
		return currentRefs.get(o);
	}

	public synchronized JTermId getOrCreate(Object o) {
		JTermId id = get(o);
		if(id == null) {
			id = new JTermId(++counter);
			currentRefs.put(o, id);
		}
		return id;	
	}
	
}
