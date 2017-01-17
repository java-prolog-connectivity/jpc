package org.jpc.engine.prolog;

import org.jpc.query.Query;
import org.jpc.term.Term;

public interface PrologDatabase {

	/**
	 * Assert a clause in the logic database. Term is asserted as the first fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	boolean asserta(Term term);
	
	/**
	 * Assert a clause in the logic database. Term is asserted as the last fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	boolean assertz(Term term);
	
	Query retract(Term term);
	
	boolean retractAll(Term term);
	
	boolean abolish(Term term);
	
	Query clause(Term head, Term body);

}
