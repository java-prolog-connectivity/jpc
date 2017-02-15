package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;
import static org.jpc.term.Functor.functor;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.term.Functor;

/**
 * A manager of term indexes.
 * In the current implementation, indexes can only be defined for compounds having ground functor names.
 * @author sergioc
 *
 */
public class IndexManager {
	
	private static final String DIRECTIVE_FUNCTOR_NAME = "directive";
	private static final IndexManager systemIndexManager;
	
	static {
		Map<Functor, List<IndexDescriptor>> functorIndexes = new HashMap<>();
		functorIndexes.put(functor(DIRECTIVE_FUNCTOR_NAME, 2), asList(IndexDescriptor.forArgumentFunctor(1)));
		systemIndexManager = new IndexManager(functorIndexes);
	}
	
	public static IndexManager getSystemIndexManager() {
		return systemIndexManager;
	}
	
	protected final Map<Functor, List<IndexDescriptor>> functorIndexes;
	
	protected static void checkIndexableFunctor(Functor functor) {
		if(!isIndexable(functor))
			throw new JpcException("Functor " + functor + " is not indexable.");
	}
	
	public static boolean isIndexable(Functor functor) {
		return functor.getArity() > 0 //indexes can only be defined for ground compounds.
				&& functor.getName().isGround();
	}
	
	public IndexManager() {
		this(new HashMap<Functor, List<IndexDescriptor>>());
	}
	
	public IndexManager(Map<Functor, List<IndexDescriptor>> functorIndexes) {
		this.functorIndexes = functorIndexes;;
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
	
}
