package org.jpc.term.expansion;

import org.jpc.term.Term;

import com.google.common.base.Optional;

public interface TermExpander {

	public Optional<Term> expand(Term term);
	
	/* //may be added when Java 8 is out supporting default methods in interfaces.
	public Optional<Term> expand(Term term) {
		return Optional.absent();
	}
	public Term expandOrSame(Term term) { 
		Term expanded = expand(term);
		return expanded!=null?expanded:term;
	}
	*/
}
