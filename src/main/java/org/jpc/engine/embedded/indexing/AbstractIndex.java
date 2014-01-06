package org.jpc.engine.embedded.indexing;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;
import org.minitoolbox.exception.NotYetImplementedException;

public abstract class AbstractIndex implements IndexChangeListener {

	protected final UpdatableIndexFunction<Term, Object> indexFunction;
	protected final Map<Object, AbstractIndex> map;
	protected final List<Clause> nonIndexedClauses;
	protected final List<Clause> allClauses;
	
	public AbstractIndex(UpdatableIndexFunction<Term, Object> indexFunction) {
		this.indexFunction = indexFunction;
		if(indexFunction.isEnabled()) {
			map = new HashMap<>();
			nonIndexedClauses = new ArrayList<>();
		} else {
			map = null;
			nonIndexedClauses = null;
		}
		allClauses = new ArrayList<>();
		indexFunction.addListener(this);
	}

	protected List<Clause> getNonIndexedClauses() {
		return nonIndexedClauses;
	}

	protected List<Clause> getAllClauses() {
		return allClauses;
	}

	protected void assertz(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			if(!isLeaf())
				nonIndexedClauses.add(clause);
		} else {
			AbstractIndex index = getOrCreateNextIndex(head);
			index.assertz(clause);
		}
		allClauses.add(clause);
	}

	protected void retract(Clause clause) {
		Term head = clause.getHead();
		if(!isIndexable(head)) {
			if(!isLeaf())
				nonIndexedClauses.remove(clause);
		} else {
			AbstractIndex index = getOrCreateNextIndex(head);
			index.retract(clause);
		}
		allClauses.remove(clause);
	}
	
	protected AbstractIndex getNextIndex(Term head) {
		Object key = indexFunction.apply(head);
		return map.get(key);
	}
	
	protected AbstractIndex getOrCreateNextIndex(Term head) {
		Object key = indexFunction.apply(head);
		AbstractIndex index = map.get(key);
		if(index == null) {
			index = new Index(getNextIndexFunctions(head));
			map.put(key, index);
		}
		return index;
	}
	
	protected boolean isIndexable(Term head) {
		try {
			return indexFunction.isEnabled() && indexFunction.apply(head) != null;
		} catch(NonIndexableTermException e) {
			return false;
		}
	}
	
	protected boolean isLeaf() {
		return !indexFunction.isEnabled();
	}
	
	public void rebuild() {
		throw new NotYetImplementedException();
	}
	
	public void reset() {
		map.clear();
		nonIndexedClauses.clear();
		allClauses.clear();
	}
	
	protected abstract List<UpdatableIndexFunction<Term, Object>> getNextIndexFunctions(Term head);
	
	@Override
	public void onIndexChange(UpdatableIndexFunction<Term, Object> updatableIndexFunction) {
		if(!allClauses.isEmpty())
			rebuild();
	}
	
	public Iterator<Clause> clausesIterator(Term head) {
		return new ClauseIterator(this, head);
	}

}
