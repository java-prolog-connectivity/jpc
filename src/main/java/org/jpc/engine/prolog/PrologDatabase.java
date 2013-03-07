package org.jpc.engine.prolog;

import org.jpc.query.Query;
import org.jpc.term.Term;

public interface PrologDatabase {

	/**
	 * Assert a clause in the logic database. Term is asserted as the first fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public abstract boolean asserta(Term term);
	
	/**
	 * Assert a clause in the logic database. Term is asserted as the last fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean assertz(Term term);
	
	public Query retract(Term term);
	
	public boolean retractAll(Term term);
	
	public boolean abolish(Term term);
	
	public Query clause(Term head, Term body);

}
