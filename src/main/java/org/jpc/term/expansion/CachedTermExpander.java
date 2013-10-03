package org.jpc.term.expansion;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class CachedTermExpander implements Function<Term, Term> {

	private Map<Term, Term> expansionCache;

	public CachedTermExpander() {
		this.expansionCache = new HashMap<>();
	}
	
	public CachedTermExpander(Map<Term, Term> map) {
		this.expansionCache = map;
	}

	protected Term getCachedExpansion(Term term) {
		return expansionCache.get(term);
	}
	
	protected void addCachedExpansion(Term sourceTerm, Term expandedTerm) {
		expansionCache.put(sourceTerm, expandedTerm);
	}
	
	protected Term doExpand(Term term) {
		return null;
	}
	
	@Override
	public Term apply(Term term) {
		Term expandedTerm = getCachedExpansion(term);
		if(expandedTerm == null) {
			expandedTerm = doExpand(term);
			if(expandedTerm != null)
				addCachedExpansion(term, expandedTerm);
		}
		return expandedTerm;
	}
	
}
