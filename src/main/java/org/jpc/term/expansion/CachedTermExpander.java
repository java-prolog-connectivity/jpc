package org.jpc.term.expansion;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Term;

import com.google.common.base.Optional;

public class CachedTermExpander implements TermExpander {

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
	
	protected Optional<Term> doExpand(Term term) {
		return Optional.absent();
	}
	
	@Override
	public Optional<Term> expand(Term term) {
		Optional<Term> optExpandedTerm = Optional.fromNullable(getCachedExpansion(term));
		if(!optExpandedTerm.isPresent()) {
			optExpandedTerm = doExpand(term);
			if(optExpandedTerm.isPresent())
				addCachedExpansion(term, optExpandedTerm.get());
		}
		return optExpandedTerm;
	}
	
}
