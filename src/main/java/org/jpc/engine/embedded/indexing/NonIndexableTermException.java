package org.jpc.engine.embedded.indexing;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class NonIndexableTermException extends RuntimeException {

	private final Term term;
	private final Function<Term, Object> indexFunction;
	
	public NonIndexableTermException(Term term, Function<Term, Object> indexFunction) {
		this.term = term;
		this.indexFunction = indexFunction;
	}
	
	@Override
	public String getMessage() {
		return "Term " + term + " is not indexable by function: " + indexFunction;
	}
	
}
