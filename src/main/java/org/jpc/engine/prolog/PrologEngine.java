package org.jpc.engine.prolog;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.Flag;
import org.jpc.exception.ExceptionHandler;
import org.jpc.query.Query;
import org.jpc.term.Term;

public interface PrologEngine extends PrologDatabase {

	//public LogtalkEngine asLogtalkEngine();
	
	public ExceptionHandler getExceptionHandler();
	
	public void registerExceptionHandler(ExceptionHandler exceptionHandler);
	
	/* ********************************************************************************************************************************
	 * CORE QUERY METHODS (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */
	
	public boolean stop();
	
	public Query simpleQuery(Term term, Jpc context);
	
	public Query query(String termString);
	
	public Query query(String termString, Jpc context);
	
	public Query query(Term terms);
	
	public Query query(Term terms, Jpc context);

	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public Term asTerm(String termString);
	
	public Term asTerm(String termString, boolean force);
	
	public List<Term> asTerms(List<String> termsString);
	
	public List<Term> asTerms(List<String> termsString, boolean force);
	
	/**
	 * escape the given string adding quotes and escaping characters if needed
	 * @param s the string to escape
	 * @return the escaped string
	 */
	public String escape(String s);
	
	
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
	
	public Query findall(Term select, Term exp, Term all);
	
	public Query setof(Term select, Term exp, Term all);
	
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
	
	public List<Term> asResourceTerms(List<String> resourceNames);
	
	public Term asResourceTerm(String resourceName);
	
	public String termSequenceToString(Term sequenceTerm);
}
