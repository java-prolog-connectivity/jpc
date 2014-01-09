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
	
	public IndexManager() {
		functorIndexes = new HashMap<>();
	}
	
	public List<IndexDescriptor> getIndexDescriptors(Functor functor) {
		return functorIndexes.get(functor);
	}
	
	public List<IndexDescriptor> getOrCreateIndexDescriptors(Functor functor) {
		List<IndexDescriptor> indexDescriptors = getIndexDescriptors(functor);
		if(indexDescriptors == null) {
			indexDescriptors = Collections.emptyList();
			functorIndexes.put(functor, indexDescriptors);
		}
		return indexDescriptors;
	}
	
	public void setIndexDescriptor(Functor functor, IndexDescriptor indexDescriptor) {
		setIndexDescriptors(functor, asList(indexDescriptor));
	}
	
	public void setIndexDescriptors(Functor functor, List<IndexDescriptor> indexDescriptors) {
		if(!functor.getName().isGround())
			throw new JpcException("Functor name " + functor.getName() + " must be ground in order to be associated with an index.");
		List<IndexDescriptor> oldIndexFunctions = getIndexDescriptors(functor);
		if(oldIndexFunctions != null) {
			//TODO: change this so indexes will be rebuild instead.
			throw new JpcException("New indexes cannot be defined for functor: " + functor + ". Entry already existing.");
		} else {
			functorIndexes.put(functor, indexDescriptors);
		}
	}
	
}
