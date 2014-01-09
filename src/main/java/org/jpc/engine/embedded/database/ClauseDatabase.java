package org.jpc.engine.embedded.database;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

public class ClauseDatabase extends ClauseList {

	private final IndexManager indexManager;
	
	public ClauseDatabase() {
		this(new IndexManager());
	}
	
	public ClauseDatabase(final IndexManager indexManager) {
		super(new IndexDescriptor(
				new UpdatableIndexFunction<Term, Object>(new FunctorIndexFunction()), //the index function maps a term to its functor name.
				/**
				 * The next indexes function makes use of the index manager to find the user-defined indexes for a given term.
				 * This function is invoked when instantiating an indexed ClauseList associated with the index of a term.
				 */
				new Function<Term, List<IndexDescriptor>>() {
					@Override
					public List<IndexDescriptor> apply(Term term) { 
						if(term instanceof Compound) { //indexes can be defined only for compounds.
							Compound compound = (Compound) term;
							return indexManager.getOrCreateIndexDescriptors(compound.getFunctor()); // <-- ground term.
						} else {
							return Collections.<IndexDescriptor>emptyList();
						}
					}
				}));
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

	public IndexManager getIndexManager() {
		return indexManager;
	}
	
}
