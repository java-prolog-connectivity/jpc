package org.jpc.term.unification;

import org.jpc.term.Term;

public class NonUnifiableException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	private final Term term1;
	private final Term term2;
	
	public NonUnifiableException(Term term1, Term term2, NonUnifiableException cause) {
		super(cause);
		this.term1 = term1;
		this.term2 = term2;
	}
	
	public NonUnifiableException(Term term1, Term term2) {
		this.term1 = term1;
		this.term2 = term2;
	}
	
	@Override
	public String getMessage() {
		return "Term " + term1 + " cannot unify with " + term2 + ".";
	}

}
