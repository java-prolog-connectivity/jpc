package org.jpc.engine.embedded.database;

import java.util.Iterator;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;

import com.google.common.base.Predicate;
import com.google.common.collect.AbstractIterator;
import com.google.common.collect.Iterators;

public class IndexedClauseIterator extends AbstractIterator<Clause> {

	private final Index index;
	private final Term head;
	private Iterator<Clause> nonIndexedIterator;
	private Iterator<Clause> indexedIterator;
	
	private Clause cachedNextNonIndexedClause;
	private Clause cachedNextIndexedClause;
	
	public IndexedClauseIterator(Index index, final Term head) {
		this.index = index;
		this.head = head;
		IndexedClauses indexedClauseList = index.getNextClauseList(head);
		if(indexedClauseList != null)
			indexedIterator = indexedClauseList.clausesIterator(head);
		nonIndexedIterator = index.getNonIndexedClauses().iterator();
		nonIndexedIterator = Iterators.filter(nonIndexedIterator, new Predicate<Clause>() {
			@Override
			public boolean apply(Clause clause) {
				return clause.getHead().isUnifiable(head);
			}
		});
	}

	@Override
	protected Clause computeNext() {
		Clause next;
		topIndexedClause();
		topNonIndexedClause();
		if(cachedNextNonIndexedClause == null) {
			if(cachedNextIndexedClause == null) {
				return this.endOfData();
			} else {
				next = cachedNextIndexedClause;
				cachedNextIndexedClause = null;
			}
		} else if(cachedNextIndexedClause == null) {
			next = cachedNextNonIndexedClause;
			cachedNextNonIndexedClause = null;
		} else { //cachedNextNonIndexedClause != null && cachedNextIndexedClause != null
			if(cachedNextNonIndexedClause.compareTo(cachedNextIndexedClause) <= 0) {
				next = cachedNextNonIndexedClause;
				cachedNextNonIndexedClause = null;
			} else {
				next = cachedNextIndexedClause;
				cachedNextIndexedClause = null;
			}
		}
		return next;
	}


	private void topNonIndexedClause() {
		if(cachedNextNonIndexedClause == null) {
			if(nonIndexedIterator != null) {
				if(nonIndexedIterator.hasNext()) {
					cachedNextNonIndexedClause = nonIndexedIterator.next();
				} else {
					nonIndexedIterator = null;
				}
			}
		}
	}
	
	private void topIndexedClause() {
		if(cachedNextIndexedClause == null) {
			if(indexedIterator != null) {
				if(indexedIterator.hasNext()) {
					cachedNextIndexedClause = indexedIterator.next();
				} else {
					indexedIterator = null;
				}
			}
		}
	}

}

