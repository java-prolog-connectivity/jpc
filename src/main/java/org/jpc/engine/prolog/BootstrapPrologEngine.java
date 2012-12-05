package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ABOLISH;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.CLAUSE;
import static org.jpc.engine.prolog.PrologConstants.ENSURE_LOADED;
import static org.jpc.engine.prolog.PrologConstants.RETRACT;
import static org.jpc.engine.prolog.PrologConstants.RETRACT_ALL;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;


public abstract class BootstrapPrologEngine implements DatabaseHandler {

	public abstract Query createQuery(TermConvertable termConvertable);

	public String escape(String s) {
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

	
	public boolean stop() {
		throw new UnsupportedOperationException();
	}
	
	public boolean flushOutput() {
		return createQuery(new Atom("flush_output")).hasSolution();
	}
	
	public String currentPrologFlag(String flagName) {
		String flagValue = null;
		Variable varFlag = new Variable("Var");
		Map<String, Term> solutions = createQuery(new Compound("current_prolog_flag", Arrays.asList(new Atom(flagName), varFlag))).oneSolution();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlag.name());
			flagValue = flagValueTerm.name();
		}
		return flagValue;
	}

	public String prologDialect() {
		return currentPrologFlag(PrologFlag.DIALECT);
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
	 * DATABASE PREDICATES PROCESSING MULTIPLE TERMS
	 * This predicates works with list of terms to which they apply the same action
	 * The rationale for having both single term database predicates and list of terms database predicates is efficiency.
	 * Depending on the underlying logic engine, the connection to it can be expensive (e.g., if JNI is used)
	 * If a logic engine supports database predicates that can process list of terms it should override these methods to use those predicates
	 * (The default implementation just iterates over the list of terms and give it as an argument to the desired database predicate)
     **********************************************************************************************************************************
     */

	
	/**
	 * Assert a list of clauses in the logic database. Terms are asserted as the first facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean asserta(List<? extends TermConvertable> termConvertables) {
		return allSucceed(LogicUtil.forEachApplyFunctor(ASSERTA, termConvertables));
	}
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	public boolean assertz(List<? extends TermConvertable> termConvertables) {
		return allSucceed(LogicUtil.forEachApplyFunctor(ASSERTZ, termConvertables));
	}	
	
	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	
	
	public boolean ensureLoaded(TermConvertable... termConvertables) {
		return ensureLoaded(asList(termConvertables));
	}
	
	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables) {
		return allSucceed(LogicUtil.forEachApplyFunctor(ENSURE_LOADED, termConvertables));
	}
	
	
	
	/* ********************************************************************************************************************************
	 * UTILITY METHODS
     **********************************************************************************************************************************
     */
	
	
	public boolean allSucceed(List<? extends TermConvertable> termConvertables) {
		boolean success = true;
		for(TermConvertable termConvertable: termConvertables) {
			if(!createQuery(termConvertable).hasSolution())
				success = false;
		}
		return success;
	}

}
