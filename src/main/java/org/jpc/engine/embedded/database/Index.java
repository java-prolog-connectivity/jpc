package org.jpc.engine.embedded.database;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;
import org.minitoolbox.exception.NotYetImplementedException;

public class Index implements IndexChangeListener {

	private final IndexDescriptor indexDescriptor;
	private final Map<Object, IndexedClauses> map;
	private final List<Clause> nonIndexedClauses;
	
	public Index(IndexDescriptor indexDescriptor) {
		this.indexDescriptor = indexDescriptor;
		map = new HashMap<>();
		nonIndexedClauses = new ArrayList<>();
		indexDescriptor.getIndexFunction().addChangeListener(this);
	}
	
	public UpdatableIndexFunction<Term, ?> getIndexFunction() {
		return indexDescriptor.getIndexFunction();
	}
	
	public List<Clause> getNonIndexedClauses() {
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
		map.clear();
		nonIndexedClauses.clear();
	}
	
	public IndexedClauses getNextClauseList(Term head) {
		Object key = getIndexFunction().apply(head);
		return map.get(key);
	}
	
	public IndexedClauses getOrCreateNextClauseList(Term head) {
		Object key = getIndexFunction().apply(head);
		IndexedClauses indexedClauses = map.get(key);
		if(indexedClauses == null) {
			indexedClauses = new IndexedClauses(indexDescriptor.getNextIndexDescriptorsFunction().apply(head));
			map.put(key, indexedClauses);
		}
		return indexedClauses;
	}
	
	public void assertz(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			nonIndexedClauses.add(clause);	
		} else {
			IndexedClauses nextClauseList = getOrCreateNextClauseList(head);
			nextClauseList.assertz(clause);
		}
	}

	public void retract(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			nonIndexedClauses.remove(clause);
		} else {
			IndexedClauses nextClauseList = getOrCreateNextClauseList(head);
			nextClauseList.retract(clause);
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
