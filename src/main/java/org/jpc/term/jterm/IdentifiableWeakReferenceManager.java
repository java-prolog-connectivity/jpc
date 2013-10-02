package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

import com.google.common.base.Optional;

public abstract class IdentifiableWeakReferenceManager<ID_TYPE, REF_TYPE> {

	private Map<ID_TYPE, WeakReference<REF_TYPE>> knownReferences;
	private ReferenceQueue<?> referenceQueue;
	
	public IdentifiableWeakReferenceManager(ReferenceQueue<?> referenceQueue){
		knownReferences = new HashMap<>();
		this.referenceQueue = referenceQueue;
	}

	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}

	public void remove(Object refId) {
		knownReferences.remove(refId);
	}
	
	public void put(ID_TYPE refId, WeakReference<REF_TYPE> weakRef) {
		knownReferences.put(refId, weakRef);
	}
	
	public <T extends WeakReference<REF_TYPE>> Optional<T> get(Object refId) {
		return (Optional<T>) Optional.fromNullable(knownReferences.get(refId));
	}
	
	public Object resolveOrThrow(Object refId) {
		WeakReference<REF_TYPE> ref;
		Optional<WeakReference<REF_TYPE>> optWeakRef = get(refId);
		if(!optWeakRef.isPresent())
			throw new RuntimeException("No such a reference: " + refId);
		else
			ref = optWeakRef.get();
		Object resolved = ref.get();
		if(resolved == null)
			throw new RuntimeException("Reference expired: " + refId);
		return resolved;
	}
	
	public boolean containsKey(Object refId) {
		return knownReferences.containsKey(refId);
	}
	
	public WeakReference<REF_TYPE> weakReference(ID_TYPE refId, REF_TYPE o) {
		Optional<WeakReference<REF_TYPE>> optWeakRef = get(refId);
		WeakReference<REF_TYPE> weakRef;
		if(!optWeakRef.isPresent()) {
			weakRef = createWeakReference(refId, o);
			put(refId, weakRef);
		} else {
			weakRef = optWeakRef.get();
			if(weakRef.get() != o)
				throw new RuntimeException("Id " + refId + " is already registered with another object."); //this should not normally happen.
		}
		return weakRef;
	}
	
	protected abstract WeakReference<REF_TYPE> createWeakReference(ID_TYPE refId, REF_TYPE o);
	
}
