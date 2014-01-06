package org.jpc.engine.embedded.indexing;

import static java.util.Arrays.asList;

import java.util.Collections;
import java.util.List;

import org.jpc.term.Term;

public class Index extends AbstractIndex {

	private final List<UpdatableIndexFunction<Term, Object>> nextIndexes;

	public Index() {
		this(new UpdatableIndexFunction<Term, Object>());
	}
	
	public Index(UpdatableIndexFunction<Term, Object> indexFunction) {
		this(asList(indexFunction));
	}
	
	/**
	 * 
	 * @param indexes a non empty array of index functions.
	 */
	public Index(List<UpdatableIndexFunction<Term, Object>> indexes) {
		super(indexes.isEmpty()?new UpdatableIndexFunction<Term, Object>():indexes.get(0));
		if(indexes.isEmpty())
			nextIndexes = Collections.emptyList();
		else
			nextIndexes = indexes.subList(1, indexes.size());
	}
	
	@Override
	protected List<UpdatableIndexFunction<Term, Object>> getNextIndexFunctions(Term head) {
		return nextIndexes;
	}

}
