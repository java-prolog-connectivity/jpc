package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ABOLISH;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.BAGOF;
import static org.jpc.engine.prolog.PrologConstants.CD;
import static org.jpc.engine.prolog.PrologConstants.CLAUSE;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_OP;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_PROLOG_FLAG;
import static org.jpc.engine.prolog.PrologConstants.ENSURE_LOADED;
import static org.jpc.engine.prolog.PrologConstants.FINDALL;
import static org.jpc.engine.prolog.PrologConstants.FLUSH_OUTPUT;
import static org.jpc.engine.prolog.PrologConstants.FORALL;
import static org.jpc.engine.prolog.PrologConstants.RETRACT;
import static org.jpc.engine.prolog.PrologConstants.RETRACT_ALL;
import static org.jpc.engine.prolog.PrologConstants.SETOF;
import static org.jpc.engine.prolog.PrologConstants.SET_PROLOG_FLAG;

import java.util.List;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;


public abstract class BootstrapPrologEngine implements DatabaseHandler {

	public abstract Query createQuery(TermConvertable termConvertable);
	
	public boolean stop() {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public Term asTerm(String termString) {
		throw new UnsupportedOperationException();
	}

	/**
	 * escape the given string adding quotes and escaping characters if needed
	 * @param s the string to escape
	 * @return the escaped string
	 */
	public String escape(String s) {
		throw new UnsupportedOperationException();
	}
	
	public boolean flushOutput() {
		return createQuery(new Atom(FLUSH_OUTPUT)).hasSolution();
	}
	
	public boolean setPrologFlag(TermConvertable flag, TermConvertable flagValue) {
		return createQuery(new Compound(SET_PROLOG_FLAG, asList(flag, flagValue))).hasSolution();
	}
	
	public Query currentPrologFlag(TermConvertable flag, TermConvertable flagValue) {
		return createQuery(new Compound(CURRENT_PROLOG_FLAG, asList(flag, flagValue)));
	}

	public Query currentOp(TermConvertable priority, TermConvertable specifier, TermConvertable operator) {
		return createQuery(new Compound(CURRENT_OP, asList(priority, specifier, operator)));
	}
	

	/* ********************************************************************************************************************************
	 * DATABASE PREDICATES
     **********************************************************************************************************************************
     */

	
	/**
	 * Assert a clause in the logic database. Term is asserted as the first fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean asserta(TermConvertable termConvertable) {
		return createQuery(new Compound(ASSERTA, asList(termConvertable))).hasSolution();
	}
	
	/**
	 * Assert a clause in the logic database. Term is asserted as the last fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean assertz(TermConvertable termConvertable) {
		return createQuery(new Compound(ASSERTZ, asList(termConvertable))).hasSolution();
	}

	@Override
	public Query retract(TermConvertable termConvertable)  {
		return createQuery(new Compound(RETRACT, asList(termConvertable)));
	}
	
	@Override
	public boolean retractAll(TermConvertable termConvertable)  {
		return createQuery(new Compound(RETRACT_ALL, asList(termConvertable))).hasSolution();
	}

	@Override
	public boolean abolish(TermConvertable termConvertable)  {
		return createQuery(new Compound(ABOLISH, asList(termConvertable))).hasSolution();
	}
	
	public Query clause(TermConvertable head, TermConvertable body)  {
		return createQuery(new Compound(CLAUSE, asList(head, body)));
	}

	

	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	
	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables) {
		return createQuery(new Compound(ENSURE_LOADED, asList(new ListTerm(termConvertables)))).hasSolution();
	}

	
	/* ********************************************************************************************************************************
	 * HIGH ORDER PREDICATES
     **********************************************************************************************************************************
     */
	public Query bagof(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return createQuery(new Compound(BAGOF, asList(select, exp, all)));
	}
	
	public Query findall(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return createQuery(new Compound(FINDALL, asList(select, exp, all)));
	}
	
	public Query setof(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return createQuery(new Compound(SETOF, asList(select, exp, all)));
	}
	
	public Query forall(TermConvertable generator, TermConvertable test) {
		return createQuery(new Compound(FORALL, asList(generator, test)));
	}
	
	/* ********************************************************************************************************************************
	 * FILE SYSTEM
     **********************************************************************************************************************************
     */
	
	public boolean cd(TermConvertable path) {
		Compound compound = new Compound(CD, asList(path));
		return createQuery(compound).hasSolution();
	}
	
}
