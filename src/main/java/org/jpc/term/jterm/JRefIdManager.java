package org.jpc.term.jterm;

import java.util.Map;

import com.google.common.base.Optional;
import com.google.common.collect.MapMaker;

/**
 * Manages identifiers for object references and warranties that such identifiers are unique per object.
 * @author sergioc
 *
 */
public class JRefIdManager {
	
	private static JRefIdManager defaultJRefIdManager = new JRefIdManager();
	
	public static JRefIdManager getJRefIdManager() {
		return defaultJRefIdManager;
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
	private Map<Object, JRefId> currentRefs = new MapMaker().weakKeys().makeMap(); 
	
	JRefIdManager(){}
	
	/**
	 * 
	 * @param o the object to which has been assigned a reference id
	 * @return the reference id
	 */
	public Optional<JRefId> get(Object o) {
		return Optional.fromNullable(currentRefs.get(o));
	}

	public synchronized JRefId getOrCreate(Object o) {
		Optional<JRefId> optJRefId = get(o);
		JRefId ref;
		if(!optJRefId.isPresent()) {
			ref = new JRefId(++counter);
			currentRefs.put(o, ref);
		} else
			ref = optJRefId.get();
		return ref;	
	}
	
}
