package org.jpc.engine.embedded.database;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;
import org.minitoolbox.exception.NotYetImplementedException;

public class Index implements IndexChangeListener {

	private final IndexDescriptor indexDescriptor;
	private final Map<Object, IndexedClauses> indexedClausesMap;
	/**
	 * Clauses that cannot be indexed (e.g., they have variables in the index term) are stored here.
	 * This set should be inspected when attempting to resolve indexed goals (which do not have variables in the index term), since the goal may unify with the (non ground) head of one of the clauses in this list.
	 */
	private final Set<Clause> nonIndexedClauses; 
	
	public Index(IndexDescriptor indexDescriptor) {
		this.indexDescriptor = indexDescriptor;
		indexedClausesMap = new HashMap<>();
		nonIndexedClauses = new TreeSet<>();
		indexDescriptor.getIndexFunction().addChangeListener(this);
	}
	
	public UpdatableIndexFunction<Term, ?> getIndexFunction() {
		return indexDescriptor.getIndexFunction();
	}
	
	/**
	 * 
	 * @return an ordered set according to the natural ordering of the clauses (by means of the Comparable interface).
	 */
	public Set<Clause> getNonIndexedClauses() {
		return nonIndexedClauses;
	}
	
	public boolean isIndexable(Term head) {
		try {
			return getIndexFunction().isEnabled() && getIndexFunction().apply(head) != null;
		} catch(NonIndexableTermException e) {
			return false;
		}
	}
	
	public void reset() {
		indexedClausesMap.clear();
		nonIndexedClauses.clear();
	}
	
	public IndexedClauses getIndexedClauses(Term head) {
		Object key = getIndexFunction().apply(head);
		return indexedClausesMap.get(key);
	}
	
	public IndexedClauses getOrCreateIndexedClauses(Term head) {
		Object key = getIndexFunction().apply(head);
		IndexedClauses indexedClauses = indexedClausesMap.get(key);
		if(indexedClauses == null) {
			indexedClauses = new IndexedClauses(indexDescriptor.getNextIndexDescriptorsFunction().apply(head));
			indexedClausesMap.put(key, indexedClauses);
		}
		return indexedClauses;
	}
	
	public void addClause(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			nonIndexedClauses.add(clause);	
		} else {
			IndexedClauses indexedClauses = getOrCreateIndexedClauses(head);
			indexedClauses.addClause(clause);
		}
	}

	public void retract(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			nonIndexedClauses.remove(clause);
		} else {
			IndexedClauses indexedClauses = getOrCreateIndexedClauses(head);
			indexedClauses.retract(clause);
		}
	}
	
	public void rebuild() {
		throw new NotYetImplementedException();
	}
	
	@Override
	public void onIndexChange(UpdatableIndexFunction<Term, Object> updatableIndexFunction) {
		rebuild();
	}

}
