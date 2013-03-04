package org.jpc.engine.prolog;

import java.util.List;

import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.query.Query;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public interface PrologEngine extends PrologDatabase {

	public LogtalkEngine asLogtalkEngine();
	
	/* ********************************************************************************************************************************
	 * CORE QUERY METHODS (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */
	
	public boolean stop();
	
	public Query query(String termString);
	
	public Query query(TermConvertable... termConvertables);
	
	public Query query(List<? extends TermConvertable> termConvertables);
	
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
	
	public boolean setPrologFlag(TermConvertable flag, TermConvertable flagValue);
	
	public boolean setPrologFlag(TermConvertable flag, String flagValue);
	
	public Query currentPrologFlag(TermConvertable flag, TermConvertable flagValue);
	
	public String currentPrologFlag(TermConvertable flag);
	
	public String prologDialect();
	
	
	/* ********************************************************************************************************************************
	 * OPERATORS
     **********************************************************************************************************************************
     */
	
	public Query currentOp(TermConvertable priority, TermConvertable specifier, TermConvertable operator);

	public boolean isBinaryOperator(String op);
	
	public boolean isUnaryOperator(String op);
	
	
	/* ********************************************************************************************************************************
	 * FILE SYSTEM
     **********************************************************************************************************************************
     */
	
	public boolean cd(TermConvertable path);
	
	public boolean cd(String path);
	
	
	/* ********************************************************************************************************************************
	 * DATABASE PREDICATES
     **********************************************************************************************************************************
     */
	
	@Override
	public boolean asserta(TermConvertable termConvertable);
	
	@Override
	public boolean assertz(TermConvertable termConvertable);

	@Override
	public Query retract(TermConvertable termConvertable);
	
	@Override
	public boolean retractAll(TermConvertable termConvertable);

	@Override
	public boolean abolish(TermConvertable termConvertable);
	
	@Override
	public Query clause(TermConvertable head, TermConvertable body);
	
	/**
	 * Assert a list of clauses in the logic database. Terms are asserted as the first facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean asserta(List<? extends TermConvertable> termConvertables);
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean assertz(List<? extends TermConvertable> termConvertables);
	
	
	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	
	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables);
	
	public boolean ensureLoaded(TermConvertable... termConvertables);

	public boolean ensureLoaded(String... resources);
	

	/* ********************************************************************************************************************************
	 * HIGH ORDER PREDICATES
     **********************************************************************************************************************************
     */
	public Query bagof(TermConvertable select, TermConvertable exp, TermConvertable all);
	
	public Query findall(TermConvertable select, TermConvertable exp, TermConvertable all);
	
	public Query setof(TermConvertable select, TermConvertable exp, TermConvertable all);
	
	public Query forall(TermConvertable generator, TermConvertable test);


	/* ********************************************************************************************************************************
	 * OTHER PREDICATES
     **********************************************************************************************************************************
     */
	
	public boolean flushOutput();
	
	/* ********************************************************************************************************************************
	 * UNIFICATION
     **********************************************************************************************************************************
     */
	
	public Term unify(TermConvertable... terms);
	
	public Term unify(List<? extends TermConvertable> terms);


	
	/* ********************************************************************************************************************************
	 * UTILITY METHODS
     **********************************************************************************************************************************
     */
	
	
	public boolean allSucceed(List<? extends TermConvertable> termConvertables);
	
	public List<Term> asResourceTerms(List<String> resourceNames);
	
	public Term asResourceTerm(String resourceName);
	
	public String termSequenceToString(TermConvertable sequenceTermConvertable);
}
