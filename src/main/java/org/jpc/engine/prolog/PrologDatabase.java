package org.jpc.engine.prolog;

import org.jpc.query.Query;
import org.jpc.term.TermConvertable;

public interface PrologDatabase {

	/**
	 * Assert a clause in the logic database. Term is asserted as the first fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public abstract boolean asserta(TermConvertable termConvertable);
	
	/**
	 * Assert a clause in the logic database. Term is asserted as the last fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean assertz(TermConvertable termConvertable);
	
	public Query retract(TermConvertable termConvertable);
	
	public boolean retractAll(TermConvertable termConvertable);
	
	public boolean abolish(TermConvertable termConvertable);
	
	public Query clause(TermConvertable head, TermConvertable body);

}
