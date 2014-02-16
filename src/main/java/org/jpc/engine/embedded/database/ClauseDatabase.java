package org.jpc.engine.embedded.database;

import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;

import com.google.common.collect.Lists;

public class ClauseDatabase extends IndexedClauses {

	private int nextLowedId = -1;
	private int nextUpperId = 0;
	
	public ClauseDatabase() {
		this(new MutableIndexManager());
	}
	
	public ClauseDatabase(final IndexManager indexManager) {
		super(IndexDescriptor.defaultRootIndexDescriptor(indexManager));
	}
	
	public void asserta(Term term) {
		addClause(new Clause(term, nextLowedId--));
	}
	
	public void assertz(Term term) {
		addClause(new Clause(term, nextUpperId++));
	}
	
	public boolean retract(Term term) {
		Term compiledTerm = term.prepareForQuery();
		Iterator<Clause> clausesIt = indexedClausesIterator(compiledTerm);
		while(clausesIt.hasNext()) {
			Clause candidateClause = clausesIt.next();
			if(term.canUnify(candidateClause.getHead())) {
				retract(candidateClause);
				return true;
			}
		}
		return false;
	}
	
	public void retractAll(Term term) {
		Term compiledTerm = term.prepareForQuery();
		Iterator<Clause> clausesIt = indexedClausesIterator(compiledTerm);
		List<Clause> clauses = Lists.newArrayList(clausesIt); //converting the iterator to a list first in order to avoid a ConcurrentModificationException.
		for(Clause candidateClause : clauses) {
			if(term.canUnify(candidateClause.getHead())) {
				retract(candidateClause);
			}
		}
	}
	
}
