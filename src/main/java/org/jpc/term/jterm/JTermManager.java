package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Compound;

import com.google.common.base.Function;

public class JTermManager {
	
	private Map<String, RefMap> knownReferences;
	private ReferenceQueue<?> referenceQueue; //weak references created by the JTermManager will be instantiated using this reference queue.
	
	public JTermManager(ReferenceQueue<?> referenceQueue) {
		this.referenceQueue = referenceQueue;
		knownReferences = new HashMap<>();
		createRefMap(JRefId.JREF_FUNCTOR, IndexArgumentFunction.firstArgumentFunction());
	}

	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}

	
	private RefMap createRefMap(String name, Function<Compound, Object> indexFunction) {
		RefMap refMap = new RefMap(indexFunction);
		putRefMap(name, refMap);
		return refMap;
	}
	
	private RefMap createRefMap(String name) {
		RefMap refMap = new RefMap();
		putRefMap(name, refMap);
		return refMap;
	}
	
	private RefMap createRefMap(Compound compound) {
		return createRefMap(compound.getNameString());
	}
	
	private void putRefMap(String name, RefMap refMap) {
		if(getRefMap(name) != null)
			throw new RuntimeException("Key: " + name + " already exists. Impossible to recreate it.");
		knownReferences.put(name, refMap);
	}
	
	private RefMap getRefMap(Compound compound) {
		return getRefMap(compound.getNameString());
	}
	
	private RefMap getRefMap(String name) {
		return knownReferences.get(name);
	}
	

	
	
	
	public boolean containsKey(Compound compound) {
		RefMap refMap = getRefMap(compound);
		if(refMap == null)
			return false;
		else
			return refMap.containsKey(compound);
	}
	
	public JTerm<?> remove(Compound compound) {
		RefMap refMap = getRefMap(compound);
		if(refMap == null)
			return null;
		else
			return refMap.remove(compound);
	}
	
	public JTerm<?> put(Compound compound, JTerm<?> jTerm) {
		RefMap refMap = getRefMap(compound);
		if(refMap == null) {
			refMap = createRefMap(compound);
		}
		return refMap.put(compound, jTerm);
	}
	
	public JTerm<?> get(Compound compound) {
		RefMap refMap = getRefMap(compound);
		if(refMap == null)
			return null;
		else
			return refMap.get(compound);
	}
	
	public JTerm<?> getOrThrow(Compound compound) {
		JTerm<?> jTerm = get(compound);
		if(jTerm == null)
			throw new RuntimeException("No object stored with reference: " + compound);
		return jTerm;
	}
	
	public Object resolveOrThrow(Compound compound) {
		JTerm<?> jTerm = getOrThrow(compound);
		Object resolved = jTerm.get();
		if(resolved == null)
			throw new RuntimeException("Reference expired: " + compound);
		return resolved;
	}


	public <T>JTerm<T> jTerm(Compound compound, T o) {
		JTerm<?> jTerm = null;
		RefMap refMap = getRefMap(compound);
		if(refMap == null)
			refMap = createRefMap(compound);
		else
			jTerm = refMap.get(compound);
		
		if(jTerm == null) {
			jTerm = new JTerm<Object>(o, (ReferenceQueue<Object>) getReferenceQueue(), compound, this);
			put(compound, jTerm);
		} else {
			if(jTerm.get() != o)
				throw new RuntimeException("Reference Id " + compound + " is already registered with another object."); //this should not normally happen.
		}
		return (JTerm<T>) jTerm;
	}
	
}
