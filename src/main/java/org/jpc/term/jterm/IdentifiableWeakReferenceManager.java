package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

public class IdentifiableWeakReferenceManager<ID_TYPE, REF_TYPE> {

	private Map<ID_TYPE, WeakReference<REF_TYPE>> knownReferences;
	private ReferenceQueue<REF_TYPE> referenceQueue;
	
	public IdentifiableWeakReferenceManager(){
		knownReferences = new HashMap<>();
		referenceQueue = new ReferenceQueue<>();
	}

	public ReferenceQueue<REF_TYPE> getReferenceQueue() {
		return referenceQueue;
	}

	public void remove(ID_TYPE refId) {
		knownReferences.remove(refId);
	}
	
	public void put(ID_TYPE refId, WeakReference<REF_TYPE> jRef) {
		knownReferences.put(refId, jRef);
	}
	
	public WeakReference<REF_TYPE> getOrThrow(ID_TYPE refId) {
		WeakReference<REF_TYPE> weakRef = knownReferences.get(refId);
		if(weakRef == null)
			throw new RuntimeException("No such a reference: " + refId);
		return weakRef;
	}
	
	public REF_TYPE resolveOrThrow(ID_TYPE refId) {
		WeakReference<REF_TYPE> weakRef = getOrThrow(refId);
		REF_TYPE object = weakRef.get();
		if(object == null)
			throw new RuntimeException("Reference expired: " + refId);
		return object;
	}
	
	public boolean containsKey(ID_TYPE refId) {
		return knownReferences.containsKey(refId);
	}
	
}
