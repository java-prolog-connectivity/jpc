package org.jpc.jterm;

import java.util.WeakHashMap;

/**
 * Manages identifiers for object references and warranties that such identifiers are unique per object
 * @author sergioc
 *
 */
public class RefIdManager {
	
	private static RefIdManager defaultRefIdManager = new RefIdManager();
	
	public static RefIdManager getDefaultRefIdManager() {
		return defaultRefIdManager;
	}
	
	private int counter = 0;
	//a weak map allows to discard an entry in the map if the key (the object associated with a reference id) is marked for garbage collection
	private WeakHashMap<Object, RefId> currentRefs = new WeakHashMap<>(); 
	
	private RefIdManager(){}
	
	/**
	 * 
	 * @param o the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public RefId get(Object o) {
		return currentRefs.get(o);
	}

	public synchronized RefId getOrCreate(Object o) {
		RefId ref = get(o);
		if(ref == null) {
			ref = new RefId(++counter);
			currentRefs.put(o, ref);
		}
		return ref;	
	}
	
}
