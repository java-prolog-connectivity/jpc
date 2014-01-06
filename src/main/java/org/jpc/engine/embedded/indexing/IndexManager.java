package org.jpc.engine.embedded.indexing;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.term.Functor;
import org.jpc.term.Term;

import com.google.common.base.Function;


public class IndexManager {

	private final Map<Functor, List<UpdatableIndexFunction<Term, Object>>> functorIndexes;
	
	public IndexManager() {
		functorIndexes = new HashMap<>();
	}
	
	public List<UpdatableIndexFunction<Term, Object>> getIndexes(Functor functor) {
		return functorIndexes.get(functor);
	}
	
	public List<UpdatableIndexFunction<Term, Object>> getOrCreateIndexes(Functor functor) {
		List<UpdatableIndexFunction<Term, Object>> indexes = getIndexes(functor);
		if(indexes == null) {
			indexes = Collections.emptyList();
			functorIndexes.put(functor, indexes);
		}
		return indexes;
	}
	
	public UpdatableIndexFunction<Term, Object> setIndex(Functor functor, Function<Term, Object> indexFunction) {
		return setIndexes(functor, asList(indexFunction)).get(0);
	}
	
	public List<UpdatableIndexFunction<Term, Object>> setIndexes(Functor functor, List<Function<Term, Object>> indexHierarchy) {
		if(!functor.getName().isGround())
			throw new JpcException("Functor name " + functor.getName() + " must be ground in order to be associated with an index.");
		List<UpdatableIndexFunction<Term, Object>> oldIndexFunctions = getIndexes(functor);
		if(oldIndexFunctions != null) {
			//TODO: change this so indexes will be rebuild instead.
			throw new JpcException("Index entries already exist for functor: " + functor + ".");
		}
		List<UpdatableIndexFunction<Term, Object>> updatableIndexHierarchy = new ArrayList<>();
		for(Function<Term, Object> index : indexHierarchy) {
			updatableIndexHierarchy.add(new UpdatableIndexFunction(index));
		}
		functorIndexes.put(functor, updatableIndexHierarchy);
		return updatableIndexHierarchy;
	}
	
}
