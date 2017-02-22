package org.jpc.engine.embedded.database;

import java.util.function.Function;

import org.jpc.term.Term;

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
