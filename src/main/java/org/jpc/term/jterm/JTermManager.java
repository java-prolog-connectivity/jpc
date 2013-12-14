package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Compound;

import com.google.common.base.Function;

/**
 * Instances of this class allow to create term representation of arbitrary Java objects.
 * This term representations can be explicitly provided by a programmer (they must be compound terms),
 * or they may be explicitly generated if no provided. In that case, references will have the form 'jref(id)', where id uniquely identifies the reference (in the current class loader).
 * @author sergioc
 *
 */
public class JTermManager {
	
	private Map<String, RefMap> knownReferences; //a map from compound names to reference maps.
	private ReferenceQueue<?> referenceQueue; //weak references created by the JTermManager will be instantiated using this reference queue.
	
	public JTermManager(ReferenceQueue<?> referenceQueue) {
		this.referenceQueue = referenceQueue;
		knownReferences = new HashMap<>();
		addIndexFunction(JRefId.JREF_FUNCTOR, IndexArgumentFunction.firstArgumentFunction());
	}

	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}

	/**
	 * Registers an index function for the given compound name.
	 * @param name the compound name associated to the given index function.
	 * @param indexFunction the index function.
	 * @return
	 */
	public RefMap addIndexFunction(String name, Function<Compound, Object> indexFunction) {
		RefMap refMap = new RefMap(indexFunction);
		putRefMap(name, refMap);
		return refMap;
	}
	
	private RefMap addDefaultIndexFunction(String name) {
		RefMap refMap = new RefMap();
		putRefMap(name, refMap);
		return refMap;
	}
	
	private RefMap createRefMap(Compound compound) {
		return addDefaultIndexFunction(compound.getNameString());
	}
	
	private RefMap getRefMap(Compound compound) {
		return getRefMap(compound.getNameString());
	}
	
	private RefMap getRefMap(String name) {
		return knownReferences.get(name);
	}
	
	private void putRefMap(String name, RefMap refMap) {
		if(getRefMap(name) != null)
			throw new RuntimeException("Key: " + name + " already exists.");
		knownReferences.put(name, refMap);
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
	
	public Object resolve(Compound compound) {
		Object resolved = null;
		JTerm<?> jTerm = get(compound);
		if(jTerm != null)
			resolved = jTerm.get();
		return resolved;
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
		RefMap refMap = getRefMap(compound); //null if there is no refmap for this kind of compound.
		if(refMap == null) {
			refMap = createRefMap(compound);
		} else {
			jTerm = refMap.get(compound); //null if the compound has not been associated with a jterm value.
		}
		
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
