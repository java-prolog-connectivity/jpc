package org.jpc.engine.embedded.database;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.jpc.engine.embedded.Clause;
import org.jpc.term.Term;

/**
 * A repository of indexed clauses.
 * @author sergioc
 *
 */
public class IndexedClauses {

	private final List<Index> indexes; //indexes defined for clauses in this repository.
	private final Set<Clause> allClauses; //if a goal cannot be resolved by one of the existing indexes, it will be resolved using the list of all clauses (this should be improved on the future).
	
	public IndexedClauses() {
		this(Collections.<IndexDescriptor>emptyList());
	}
	
	public IndexedClauses(IndexDescriptor indexDescriptor) {
		this(asList(indexDescriptor));
	}
	
	public IndexedClauses(List<IndexDescriptor> indexDescriptors) {
		allClauses = new TreeSet<>(); //to keep the natural ordering of the clauses (by means of the Comparable interface).
		indexes = new ArrayList<>();
		for(IndexDescriptor indexDescriptor : indexDescriptors) {
			indexes.add(new Index(indexDescriptor));
		}
	}

	protected void addClause(Clause clause) {
		for(Index index : indexes) {
			index.addClause(clause);
		}
		allClauses.add(clause);
	}

	public void retract(Clause clause) {
		for(Index index : indexes) {
			index.retract(clause);
		}
		allClauses.remove(clause);
	}

	public void reset() {
		for(Index index : indexes) {
			index.reset();
		}
		allClauses.clear();
	}
	
	/**
	 * This method answers an iterator of clauses which heads "may" unify with the parameter according to the existing indexes applicable to it.
	 * The rationale of implementing it that way instead of answering only clauses which heads unify with the argument, is to avoid duplicated unification tasks.
	 * This is because callers of this method may need to unify again the head of the clauses against the term parameter (for example, to find out the bindings of the variables in the unified term). 
	 * @param head the term that may unify with the head of the clauses.
	 * @return an iterator of clauses which heads may unify with the given term.
	 */
	public Iterator<Clause> indexedClausesIterator(final Term head) {
		Iterator<Clause> it;
		Index matchedIndex = null;
		
		for(Index index : indexes) {
			if(index.isIndexable(head)) {
				matchedIndex = index;
				break;
			}
		}
		if(matchedIndex != null) {
			it = new IndexedClauseIterator(matchedIndex, head);
		} else {
			it =  allClauses.iterator();
		}
		return it;
	}

}
