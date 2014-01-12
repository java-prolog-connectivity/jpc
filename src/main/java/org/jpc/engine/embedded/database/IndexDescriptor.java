package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.Collections;
import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Function;
import com.google.common.base.Functions;

public class IndexDescriptor {
	
	/**
	 * 
	 * @param argPos the term argument position.
	 * @param indexManager an index manager.
	 * @return an IndexDescriptor based on the defined index of a term argument in the given position.
	 */
	public static IndexDescriptor argumentIndexDescriptor(final int argPos, IndexManager indexManager) {
		Function<Term, Term> termArgumentFunction = new Function<Term, Term>() {
			@Override
			public Term apply(Term term) {
				return term.arg(argPos);
			}
		};
		return indexDescriptorAdapter(defaultRootIndexDescriptor(indexManager), termArgumentFunction);
	}
	
	public static IndexDescriptor indexDescriptorAdapter(IndexDescriptor indexDescriptor, Function<Term, Term> adapterFunction) {
		Function<Term, Object> indexFunction = Functions.compose(indexDescriptor.getIndexFunction(), adapterFunction);
		Function<Term, List<IndexDescriptor>> indexDescriptorFunction = Functions.compose(indexDescriptor.getNextIndexDescriptorsFunction(), adapterFunction);
		return new IndexDescriptor(indexFunction, indexDescriptorFunction);
	}
	
	public static IndexDescriptor defaultRootIndexDescriptor(final IndexManager indexManager) {
		return new IndexDescriptor(
				new UpdatableIndexFunction<Term, Object>(new FunctorIndexFunction()), //the index function maps a term to its functor name.
				/**
				 * The next indexes function makes use of the index manager to find the user-defined indexes for a given term.
				 * This function is invoked when instantiating an indexed IndexedClauses associated with the index of a term.
				 */
				new Function<Term, List<IndexDescriptor>>() {
					@Override
					public List<IndexDescriptor> apply(Term term) { 
						if(term instanceof Compound) { //indexes can be defined only for compounds.
							Compound compound = (Compound) term;
							return indexManager.getOrCreateIndexDescriptors(compound.getFunctor()); //functor should be ground (otherwise FunctorIndexFunction would have complained).
						} else {
							return Collections.<IndexDescriptor>emptyList();
						}
					}
				});
	}
	
	public static IndexDescriptor forFunctions(final List<Function<Term, Object>> indexFunctions) {
		UpdatableIndexFunction<Term, Object> indexFunction = new UpdatableIndexFunction<>(indexFunctions.get(0));
		if(indexFunctions.size() == 1) {
			return new IndexDescriptor(indexFunction);
		} else {
			return new IndexDescriptor(indexFunction, new Function<Term, List<IndexDescriptor>>() {
				@Override
				public List<IndexDescriptor> apply(Term term) {
					return asList(forFunctions(indexFunctions.subList(1, indexFunctions.size())));
				}
			});
		}
	}
	
	
	private final UpdatableIndexFunction<Term, Object> indexFunction;
	private final Function<Term, List<IndexDescriptor>> nextIndexDescriptorsFunction;
	
	public IndexDescriptor(Function<Term, Object> indexFunction) {
		this(new UpdatableIndexFunction<>(indexFunction));
	}
	
	public IndexDescriptor(Function<Term, Object> indexFunction, Function<Term, List<IndexDescriptor>> nextIndexDescriptorsFunction) {
		this(new UpdatableIndexFunction<>(indexFunction), nextIndexDescriptorsFunction);
	}
	
	public IndexDescriptor(UpdatableIndexFunction<Term, Object> indexFunction) {
		this(indexFunction, new Function<Term, List<IndexDescriptor>>() {
			@Override
			public List<IndexDescriptor> apply(Term term) {
				return Collections.<IndexDescriptor>emptyList();
			}
		});
	}
	
	public IndexDescriptor(UpdatableIndexFunction<Term, Object> indexFunction, Function<Term, List<IndexDescriptor>> nextIndexDescriptorsFunction) {
		this.indexFunction = indexFunction;
		this.nextIndexDescriptorsFunction = nextIndexDescriptorsFunction;
	}

	public UpdatableIndexFunction<Term, Object> getIndexFunction() {
		return indexFunction;
	}

	public Function<Term, List<IndexDescriptor>> getNextIndexDescriptorsFunction() {
		return nextIndexDescriptorsFunction;
	}

}
