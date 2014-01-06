package org.jpc.engine.embedded.indexing;

import java.util.Iterator;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;

import com.google.common.base.Predicate;
import com.google.common.collect.AbstractIterator;
import com.google.common.collect.Iterators;

public class ClauseIterator extends AbstractIterator<Clause> {

	private final AbstractIndex index;
	private final Term head;
	private Iterator<Clause> nonIndexedIterator;
	private Iterator<Clause> indexedIterator;
	
	private Clause cachedNextNonIndexedClause;
	private Clause cachedNextIndexedClause;
	
	public ClauseIterator(AbstractIndex index, final Term head) {
		this.index = index;
		this.head = head;
		if(index.isIndexable(head)) {
			indexedIterator = index.getNextIndex(head).clausesIterator(head);
			nonIndexedIterator = index.getNonIndexedClauses().iterator();
		} else {
			indexedIterator = null;
			nonIndexedIterator = index.getAllClauses().iterator();
		}
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

