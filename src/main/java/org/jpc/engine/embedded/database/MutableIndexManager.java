package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.List;

import org.jpc.JpcException;
import org.jpc.term.Functor;

public class MutableIndexManager extends IndexManager {
	
	public synchronized List<IndexDescriptor> getIndexDescriptors(Functor functor) {
		return super.getIndexDescriptors(functor);
	}
	
	public synchronized List<IndexDescriptor> getOrCreateIndexDescriptors(Functor functor) {
		return super.getOrCreateIndexDescriptors(functor);
	}
	
	public synchronized void removeIndexDescriptor(Functor functor) {
		functorIndexes.remove(functor);
	}
	
	public synchronized void setIndexDescriptor(Functor functor, IndexDescriptor indexDescriptor) {
		setIndexDescriptors(functor, asList(indexDescriptor));
	}
	
	public synchronized void setIndexDescriptors(Functor functor, List<IndexDescriptor> indexDescriptors) {
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