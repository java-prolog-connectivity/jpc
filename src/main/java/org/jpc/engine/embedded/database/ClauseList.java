package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;

public class ClauseList {

	private final List<Clause> allClauses;
	private final List<Index> indexes;
	
	public static ClauseList forIndexManager(final IndexManager indexManager) {
		return new ClauseList(new IndexDescriptor(
				new UpdatableIndexFunction<Term, Object>(new FunctorIndexFunction()), 
				new Function<Term, List<IndexDescriptor>>() {
					@Override
					public List<IndexDescriptor> apply(Term term) {
						if(term instanceof Compound) {
							Compound compound = (Compound) term;
							return indexManager.getOrCreateIndexDescriptors(compound.getFunctor());
						} else {
							return Collections.<IndexDescriptor>emptyList();
						}
					}
				}));
	}
	
	public ClauseList() {
		this(Collections.<IndexDescriptor>emptyList());
	}
	
	public ClauseList(IndexDescriptor indexDescriptor) {
		this(asList(indexDescriptor));
	}
	
	public ClauseList(List<IndexDescriptor> indexDescriptors) {
		allClauses = new ArrayList<>();
		indexes = new ArrayList<>();
		for(IndexDescriptor indexDescriptor : indexDescriptors) {
			indexes.add(new Index(indexDescriptor));
		}
	}

	public void assertz(Clause clause) {
		for(Index index : indexes) {
			index.assertz(clause);
		}
		allClauses.add(clause);
	}

	public void retract(Clause clause) {
		for(Index index : indexes) {
			index.retract(clause);
		}
		allClauses.remove(clause);
	}

	public void reset() {
		for(Index index : indexes) {
			index.reset();
		}
		allClauses.clear();
	}
	
	public Iterator<Clause> clausesIterator(final Term compiledHead) {
		Iterator<Clause> it;
		Index matchedIndex = null;
		
		for(Index index : indexes) {
			if(index.isIndexable(compiledHead)) {
				matchedIndex = index;
				break;
			}
		}
		if(matchedIndex != null) {
			it = new IndexedClauseIterator(matchedIndex, compiledHead);
		} else {
			it =  Iterators.filter(allClauses.iterator(), new Predicate<Clause>() {
				@Override
				public boolean apply(Clause clause) {
					return clause.getHead().isUnifiable(compiledHead);
				}
			});
		}
		return it;
	}

}
