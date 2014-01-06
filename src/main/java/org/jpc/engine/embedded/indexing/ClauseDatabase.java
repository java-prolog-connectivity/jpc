package org.jpc.engine.embedded.indexing;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;

public class ClauseDatabase extends AbstractIndex {

	private final IndexManager indexManager;
	
	public ClauseDatabase(IndexManager indexManager) {
		super(new UpdatableIndexFunction<Term, Object>(new CompoundFunctorIndexFunction()));
		this.indexManager = indexManager;
	}
	
	public void assertz(Term term) {
		assertz(new Clause(term));
	}
	
	public boolean retract(Term term) {
		Term compiledTerm = term.compileForQuery();
		Iterator<Clause> clausesIt = clausesIterator(compiledTerm);
		if(clausesIt.hasNext()) {
			retract(clausesIt.next());
			return true;
		} else
			return false;
	}
	
	public void retractAll(Term term) {
		Term compiledTerm = term.compileForQuery();
		Iterator<Clause> clausesIt = clausesIterator(compiledTerm);
		List<Clause> clauses = Lists.newArrayList(clausesIt); //converting the iterator to a list first in order to avoid a ConcurrentModificationException.
		for(Clause clause : clauses) {
			retract(clause);
		}

	}
	
	@Override
	protected List<UpdatableIndexFunction<Term, Object>> getNextIndexFunctions(Term term) {
		if(term instanceof Compound) {
			Compound compound = (Compound) term;
			return indexManager.getOrCreateIndexes(compound.getFunctor());
		} else {
			return Collections.<UpdatableIndexFunction<Term, Object>>emptyList();
		}
	}
	
	@Override
	public Iterator<Clause> clausesIterator(final Term compiledHead) {
		Iterator<Clause> it;
		AbstractIndex index = null;
		try {
			index = getNextIndex(compiledHead);
			if(index != null) {
				it = index.clausesIterator(compiledHead);
			} else {
				it = Iterators.emptyIterator();
			}
		} catch(NonIndexableTermException e) {
			it = Iterators.filter(getAllClauses().iterator(), new Predicate<Clause>() {
				@Override
				public boolean apply(Clause clause) {
					return clause.getHead().isUnifiable(compiledHead);
				}
			});
		}
		return it;
	}
	
}
