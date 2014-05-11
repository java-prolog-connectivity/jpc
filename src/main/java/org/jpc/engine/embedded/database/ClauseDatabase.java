package org.jpc.engine.embedded.database;

import java.util.Iterator;
import java.util.List;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;
import org.jpc.term.compiler.Environment;

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
		addClause(new Clause(term, new Environment(nextLowedId--)));
	}
	
	public void assertz(Term term) {
		addClause(new Clause(term, new Environment(nextUpperId++)));
	}
	
	public boolean retract(Term term) {
		Term compiledTerm = term.compile(true);
		Iterator<Clause> clausesIt = indexedClausesIterator(compiledTerm);
		while(clausesIt.hasNext()) {
			Clause candidateClause = clausesIt.next();
			Term compiledHead = candidateClause.getHead().prepareForFrame();
			if(compiledTerm.canUnify(compiledHead)) {
				retract(candidateClause);
				return true;
			}
		}
		return false;
	}
	
	public void retractAll(Term term) {
		Term compiledTerm = term.compile(true);
		Iterator<Clause> clausesIt = indexedClausesIterator(compiledTerm);
		List<Clause> clauses = Lists.newArrayList(clausesIt); //converting the iterator to a list first in order to avoid a ConcurrentModificationException.
		for(Clause candidateClause : clauses) {
			Term compiledHead = candidateClause.getHead().prepareForFrame();
			if(compiledTerm.canUnify(compiledHead)) {
				retract(candidateClause);
			}
		}
	}
	
}
