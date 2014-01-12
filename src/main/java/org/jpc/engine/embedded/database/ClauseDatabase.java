package org.jpc.engine.embedded.database;

import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;

import com.google.common.collect.Lists;

public class ClauseDatabase extends IndexedClauses {

	private final IndexManager indexManager;
	
	public ClauseDatabase() {
		this(new IndexManager());
	}
	
	public ClauseDatabase(final IndexManager indexManager) {
		super(IndexDescriptor.defaultRootIndexDescriptor(indexManager));
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
