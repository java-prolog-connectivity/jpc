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
	 * @return the thread model of the Prolog engine. This specifies if the engine can manage or not concurrent requests.
	 */
	ThreadModel threadModel();
	
	boolean command(String command);
	
	boolean command(String command, List<?> arguments);
	
	boolean command(String command, Jpc context);
	
	boolean command(String command, List<?> arguments, Jpc context);

	Query query(String goalString);
	
	Query query(String goalString, List<?> arguments);

	Query query(String goalString, Jpc context);

	Query query(String goalString, List<?> arguments, Jpc context);

	Query query(String goalString, List<?> arguments, boolean errorHandledQuery, Jpc context);
	
	Query query(Term goal);
	
	Query query(Term goal, Jpc context);
	
	Query query(Term goal, List<?> arguments, boolean errorHandledQuery, Jpc context);
	
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
	Term asTerm(String termString);
	
	Term asTerm(String termString, Jpc context);
	
	List<Term> asTerms(List<String> termsString);
	
	List<Term> asTerms(List<String> termsString, Jpc context);
	
	/* ********************************************************************************************************************************
	 * FLAGS
     **********************************************************************************************************************************
     */
	
	boolean setPrologFlag(Term flag, Term flagValue);
	
	boolean setPrologFlag(Flag flag, String flagValue);
	
	Query currentPrologFlag(Term flag, Term flagValue);
	
	Query currentPrologFlag(Flag flag, String flagValue);
	
	String currentPrologFlag(Flag flag);
	
	String prologDialect();
	
	
	/* ********************************************************************************************************************************
	 * OPERATORS
     **********************************************************************************************************************************
     */
	
	OperatorsContext getOperatorsContext();
	
	Query currentOp(Term priority, Term specifier, Term operator);

	boolean isBinaryOperator(String op);
	
	boolean isUnaryOperator(String op);
	
	
	/* ********************************************************************************************************************************
	 * FILE SYSTEM
     **********************************************************************************************************************************
     */
	
	boolean cd(Term path);
	
	boolean cd(String path);
	
	
	/* ********************************************************************************************************************************
	 * DATABASE PREDICATES
     **********************************************************************************************************************************
     */
	
	@Override
	boolean asserta(Term term);
	
	@Override
	boolean assertz(Term term);

	@Override
	Query retract(Term term);
	
	@Override
	boolean retractAll(Term term);

	@Override
	boolean abolish(Term term);
	
	@Override
	Query clause(Term head, Term body);
	
	/**
	 * Assert a list of clauses in the logic database. Terms are asserted as the first facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	boolean asserta(List<? extends Term> terms);
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	boolean assertz(List<? extends Term> terms);
	
	
	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	
	boolean ensureLoaded(List<? extends Term> terms);
	
	boolean ensureLoaded(Term... terms);

	boolean ensureLoaded(String... resources);
	

	/* ********************************************************************************************************************************
	 * HIGH ORDER PREDICATES
     **********************************************************************************************************************************
     */
	Query bagof(Term select, Term exp, Term all);
	
	Term bagof(Term select, Term exp);
	
	Query findall(Term select, Term exp, Term all);
	
	Term findall(Term select, Term exp);
	
	Query setof(Term select, Term exp, Term all);
	
	Term setof(Term select, Term exp);
	
	Query forall(Term generator, Term test);


	/* ********************************************************************************************************************************
	 * OTHER PREDICATES
     **********************************************************************************************************************************
     */
	
	boolean flushOutput();
	
	
	/* ********************************************************************************************************************************
	 * UNIFICATION
     **********************************************************************************************************************************
     */
	
	Term unify(Term... terms);
	
	Term unify(List<? extends Term> terms);


	/* ********************************************************************************************************************************
	 * UTILITY METHODS
     **********************************************************************************************************************************
     */
	
	boolean allSucceed(List<? extends Term> terms);
	
	
	/* ********************************************************************************************************************************
	 * Logtalk
     **********************************************************************************************************************************
     */
	
	LogtalkEngine withLogtalk();
	
	LogtalkEngine asLogtalkEngine();

	
	//The methods below are intended to be invoked from the Prolog side only with the purpose of configuring JPC.
	/* ********************************************************************************************************************************
	 * CONFIGURATION FROM THE PROLOG SIDE
     **********************************************************************************************************************************
     */
	
	void loadJpc();
	
	void loadJpcForLogtalk();
}
