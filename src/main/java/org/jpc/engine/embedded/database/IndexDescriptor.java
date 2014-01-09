package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.Collections;
import java.util.List;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class IndexDescriptor {

	private final UpdatableIndexFunction<Term, Object> indexFunction;
	private final Function<Term, List<IndexDescriptor>> nextIndexDescriptorsFunction;
	
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
