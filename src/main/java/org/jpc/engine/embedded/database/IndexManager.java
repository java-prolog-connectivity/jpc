package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.term.Functor;


public class IndexManager {

	private final Map<Functor, List<IndexDescriptor>> functorIndexes;
	
	private static void checkIndexableFunctor(Functor functor) {
		if(!isIndexable(functor))
			throw new JpcException("Functor " + functor + " is not indexable.");
	}
	
	public static boolean isIndexable(Functor functor) {
		return functor.getArity() > 0 //only compounds can be indexed.
				&& functor.getName().isGround();
	}
	
	public IndexManager() {
		functorIndexes = new HashMap<>();
	}
	
	public List<IndexDescriptor> getIndexDescriptors(Functor functor) {
		return functorIndexes.get(functor);
	}
	
	public List<IndexDescriptor> getOrCreateIndexDescriptors(Functor functor) {
		List<IndexDescriptor> indexDescriptors = getIndexDescriptors(functor);
		if(indexDescriptors == null) {
			checkIndexableFunctor(functor);
			indexDescriptors = Collections.emptyList();
			functorIndexes.put(functor, indexDescriptors);
		}
		return indexDescriptors;
	}
	
	public void setIndexDescriptor(Functor functor, IndexDescriptor indexDescriptor) {
		setIndexDescriptors(functor, asList(indexDescriptor));
	}
	
	public void setIndexDescriptors(Functor functor, List<IndexDescriptor> indexDescriptors) {
		checkIndexableFunctor(functor);
		List<IndexDescriptor> oldIndexFunctions = getIndexDescriptors(functor);
		if(oldIndexFunctions != null) {
			//TODO: change this so indexes will be rebuild instead.
			throw new JpcException("New indexes cannot be defined for functor: " + functor + ". Entry already exists.");
		} else {
			functorIndexes.put(functor, indexDescriptors);
		}
	}
	

	
}
