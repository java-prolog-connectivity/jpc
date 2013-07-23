package org.jpc.jterm;

import java.lang.ref.ReferenceQueue;
import java.util.HashMap;
import java.util.Map;

public class RefManager {

	private static RefManager defaultRefManager;
	
	static {
		defaultRefManager = new RefManager();
		new WeakReferencesCleaner(defaultRefManager, Thread.MAX_PRIORITY).start();
	}
	
	public static RefManager getDefaultRefManager() {
		return defaultRefManager;
	}
	
	public static JRef jRef(Object o) {
		return defaultRefManager.newJRef(o);
	}
	
	private Map<RefId, JRef> knownReferences;
	
	private ReferenceQueue<Object> referenceQueue;
	
	private RefManager(){
		referenceQueue = new ReferenceQueue<>();
		knownReferences = new HashMap<>();
	}

	public ReferenceQueue<Object> getReferenceQueue() {
		return referenceQueue;
	}

	public JRef newJRef(Object o) {
		JRef jRef = new JRef(o, referenceQueue);
		RefId refId = jRef.getRefId();
		if(!containsKey(refId))
			put(refId, jRef);
		return jRef;
	}
	
	public void remove(RefId refId) {
		knownReferences.remove(refId);
	}
	
	public void put(RefId refId, JRef jRef) {
		knownReferences.put(refId, jRef);
	}
	
	public <T> T getOrThrow(RefId refId) {
		T object = null;
		JRef jRef = knownReferences.get(refId);
		if(jRef == null || jRef.get() == null)
			throw new RuntimeException("No such a reference: " + refId);
		object = (T) jRef.get();
		return object;
	}
	
	public boolean containsKey(RefId refId) {
		return knownReferences.containsKey(refId);
	}
	
}
