package org.jpc.engine.prolog;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.query.Query;
import org.jpc.term.Term;

/**
 * An abstract interface for a Prolog engine session.
 * Depending on the underlying driver library, multiple sessions can be spawned in the JVM (e.g., InterProlog), or only one (e.g., JPL). 
 * @author sergioc
 *
 */
public interface PrologEngine extends PrologDatabase {

	public LogtalkEngine asLogtalkEngine();
	
	/* ********************************************************************************************************************************
	 * CORE QUERY METHODS (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */
	
	/**
	 * Close the logic engine and free associated resources.
	 * Once the session is not required it can be closed. However, some drivers do not allow this and will throw an UnsupportedOperationException instead
	 * @return
	 */
	public void close();

	/**
	 * @return true if the Prolog engine can be closed (i.e., its resources can be freed). false otherwise.
	 */
	public boolean isCloseable();
	
	/**
	 * @return true if the Prolog engine can manage concurrent requests (e.g., multiple queries open at the same time). false otherwise.
	 */
	public boolean isMultiThreaded();
	
	public boolean command(String command);
	
	public boolean command(String command, boolean errorHandledQuery);
	
	public boolean command(String command, Jpc context);
	
	public Query query(String termString);
	
	public Query query(Term terms);
	
	public Query query(String termString, boolean errorHandledQuery);
	
	public Query query(Term terms, boolean errorHandledQuery);

	public Query query(String termString, Jpc context);

	public Query query(Term terms, Jpc context);

	public Query query(String termString, boolean errorHandledQuery, Jpc context);
	
	public Query query(Term term, boolean errorHandledQuery, Jpc context);
	
	/**
	 * 
	 * @return a non-instrumented query
	 */
	Query basicQuery(Term term, boolean errorHandledQuery, Jpc context);
	
	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public Term asTerm(String termString);
	
	public Term asTerm(String termString, Jpc context);
	
	public List<Term> asTerms(List<String> termsString);
	
	public List<Term> asTerms(List<String> termsString, Jpc context);
	
	/* ********************************************************************************************************************************
	 * FLAGS
     **********************************************************************************************************************************
     */
	
	public boolean setPrologFlag(Term flag, Term flagValue);
	
	public boolean setPrologFlag(Flag flag, String flagValue);
	
	public Query currentPrologFlag(Term flag, Term flagValue);
	
	public Query currentPrologFlag(Flag flag, String flagValue);
	
	public String currentPrologFlag(Flag flag);
	
	public String prologDialect();
	
	
	/* ********************************************************************************************************************************
	 * OPERATORS
     **********************************************************************************************************************************
     */
	
	public OperatorsContext getOperatorsContext();
	
	public Query currentOp(Term priority, Term specifier, Term operator);

	public boolean isBinaryOperator(String op);
	
	public boolean isUnaryOperator(String op);
	
	
	/* ********************************************************************************************************************************
	 * FILE SYSTEM
     **********************************************************************************************************************************
     */
	
	public boolean cd(Term path);
	
	public boolean cd(String path);
	
	
	/* ********************************************************************************************************************************
	 * DATABASE PREDICATES
     **********************************************************************************************************************************
     */
	
	@Override
	public boolean asserta(Term term);
	
	@Override
	public boolean assertz(Term term);

	@Override
	public Query retract(Term term);
	
	@Override
	public boolean retractAll(Term term);

	@Override
	public boolean abolish(Term term);
	
	@Override
	public Query clause(Term head, Term body);
	
	/**
	 * Assert a list of clauses in the logic database. Terms are asserted as the first facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean asserta(List<? extends Term> terms);
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean assertz(List<? extends Term> terms);
	
	
	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	
	public boolean ensureLoaded(List<? extends Term> terms);
	
	public boolean ensureLoaded(Term... terms);

	public boolean ensureLoaded(String... resources);
	

	/* ********************************************************************************************************************************
	 * HIGH ORDER PREDICATES
     **********************************************************************************************************************************
     */
	public Query bagof(Term select, Term exp, Term all);
	
	public Term bagof(Term select, Term exp);
	
	public Query findall(Term select, Term exp, Term all);
	
	public Term findall(Term select, Term exp);
	
	public Query setof(Term select, Term exp, Term all);
	
	public Term setof(Term select, Term exp);
	
	public Query forall(Term generator, Term test);


	/* ********************************************************************************************************************************
	 * OTHER PREDICATES
     **********************************************************************************************************************************
     */
	
	public boolean flushOutput();
	
	/* ********************************************************************************************************************************
	 * UNIFICATION
     **********************************************************************************************************************************
     */
	
	public Term unify(Term... terms);
	
	public Term unify(List<? extends Term> terms);


	
	/* ********************************************************************************************************************************
	 * UTILITY METHODS
     **********************************************************************************************************************************
     */
	
	
	public boolean allSucceed(List<? extends Term> terms);

}
