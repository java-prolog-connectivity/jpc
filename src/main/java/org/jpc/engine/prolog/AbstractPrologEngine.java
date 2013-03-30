package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.JpcPreferences.JPC_VAR_PREFIX;
import static org.jpc.engine.prolog.PrologConstants.ABOLISH;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.ATOM_CHARS;
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
import static org.jpc.term.ListTerm.listTerm;
import static org.jpc.term.Variable.ANONYMOUS_VAR;
import static org.jpc.util.LogicUtil.termSequence;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.jpc.Jpc;
import org.jpc.engine.Flag;
import org.jpc.query.ExceptionHandledQuery;
import org.jpc.query.Query;
import org.jpc.query.QuerySolutionToTermFunction;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;

public abstract class AbstractPrologEngine implements PrologEngine {
	
	private static final String ALL_RESULTS_VAR = JPC_VAR_PREFIX + "ALL_RESULTS";
	
	public AbstractPrologEngine() {
	}

//	public LogtalkEngine asLogtalkEngine() {
//		return new LogtalkEngine(this);
//	}

	/* ********************************************************************************************************************************
	 * CORE METHODS (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */

	@Override
	public abstract boolean interrupt();
	
	@Override
	public abstract boolean shutdown();

	@Override
	public boolean command(String command) {
		return query(command).hasSolution();
	}
	
	@Override
	public boolean command(String command, boolean errorHandledQuery) {
		return query(command, errorHandledQuery).hasSolution();
	}
	
	@Override
	public boolean command(String command, Jpc context) {
		return query(command, context).hasSolution();
	}
	
	@Override
	public final Query query(String termString) {
		return query(termString, new Jpc());
	}
	
	@Override
	public final Query query(Term terms) {
		return query(terms, new Jpc());
	}
	
	@Override
	public final Query query(String termString, boolean errorHandledQuery) {
		return query(termString, errorHandledQuery, new Jpc());
	}
	
	@Override
	public final Query query(Term terms, boolean errorHandledQuery) {
		return query(terms, errorHandledQuery, new Jpc());
	}
	
	@Override
	public final Query query(String termString, Jpc context) {
		return query(termString, true, context);
	}
	
	@Override
	public final Query query(Term terms, Jpc context) {
		return query(terms, true, context);
	}
	
	@Override
	public final Query query(String termString, boolean errorHandledQuery, Jpc context) {
		return query(asTerm(termString), errorHandledQuery, context);
	}
 
	@Override
	public Query query(Term term, boolean errorHandledQuery, Jpc context) {
		Query query;
		if(errorHandledQuery)
			query = new ExceptionHandledQuery(basicQuery(term, true, context));
		else
			query = basicQuery(term, false, context);
		return query;
	}
	
	protected abstract Query basicQuery(Term term, boolean errorHandledQuery, Jpc context);
	
//	public Term asTerm(String termString, boolean force) {
//		try {
//			return asTerm(termString); //to be provided by subclasses
//		} catch(Exception e) {
//			if(force)
//				return new Atom(termString);
//			else
//				throw e;
//		}
//	}
	

	@Override
	public List<Term> asTerms(List<String> termsString) {
		List<Term> terms = new ArrayList<>();
		for(String s : termsString)
			terms.add(asTerm(s));
		return terms;
	}
	
//	public List<Term> asTerms(List<String> termsString, boolean force) {
//		List<Term> terms = new ArrayList<>();
//		for(String s : termsString)
//			terms.add(asTerm(s, force));
//		return terms;
//	}
	
	
	/* ********************************************************************************************************************************
	 * FLAGS
     **********************************************************************************************************************************
     */
	
	@Override
	public boolean setPrologFlag(Term flag, Term flagValue) {
		return query(new Compound(SET_PROLOG_FLAG, asList(flag, flagValue))).hasSolution();
	}
	
	@Override
	public boolean setPrologFlag(Flag flag, String flagValue) {
		return setPrologFlag(flag.asTerm(), new Atom(flagValue));
	}
	
	@Override
	public Query currentPrologFlag(Term flag, Term flagValue) {
		return query(new Compound(CURRENT_PROLOG_FLAG, asList(flag, flagValue)));
	}
	
	@Override
	public Query currentPrologFlag(Flag flag, String flagValue) {
		return currentPrologFlag(flag.asTerm(), new Atom(flagValue));
	}
	
	@Override
	public String currentPrologFlag(Flag flag) {
		String flagValue = null;
		Variable varFlagValue = new Variable("Var");
		Map<String, Term> solutions = currentPrologFlag(flag.asTerm(), varFlagValue).oneSolution();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlagValue.getName());
			flagValue = flagValueTerm.getName();
		}
		return flagValue;
	}
	
	@Override
	public String prologDialect() {
		return currentPrologFlag(PrologFlag.DIALECT);
	}
	
	
	/* ********************************************************************************************************************************
	 * OPERATORS
     **********************************************************************************************************************************
     */
	
	@Override
	public Query currentOp(Term priority, Term specifier, Term operator) {
		return query(new Compound(CURRENT_OP, asList(priority, specifier, operator)));
	}

	@Override
	public boolean isBinaryOperator(String op) {
		return query(termSequence(
				new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Variable("Type"), new Atom(op))), 
				new Compound(ATOM_CHARS, Arrays.<Term>asList(new Variable("Type"), listTerm(ANONYMOUS_VAR, new Atom("f"), ANONYMOUS_VAR)))
			)).hasSolution();
		//return createQuery("current_op(_, Type, '" + op + "'), atom_chars(Type, [_, f, _])").hasSolution();
	}
	
	@Override
	public boolean isUnaryOperator(String op) {
		return query(termSequence(
				new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Variable("Type"), 
				new Atom(op))), new Compound(ATOM_CHARS, asList(new Variable("Type"), listTerm(new Atom("f"), ANONYMOUS_VAR)))
			)).hasSolution();
		//return createQuery("current_op(_, Type, '" + op + "'), atom_chars(Type, [f, _])").hasSolution();
	}
	
	
	/* ********************************************************************************************************************************
	 * FILE SYSTEM
     **********************************************************************************************************************************
     */
	
	@Override
	public boolean cd(Term path) {
		Compound compound = new Compound(CD, asList(path));
		return query(compound).hasSolution();
	}
	
	@Override
	public boolean cd(String path) {
		return cd(new Atom(path));
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
	public boolean asserta(Term term) {
		return query(new Compound(ASSERTA, asList(term))).hasSolution();
	}
	
	/**
	 * Assert a clause in the logic database. Term is asserted as the last fact or rule of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean assertz(Term term) {
		return query(new Compound(ASSERTZ, asList(term))).hasSolution();
	}

	@Override
	public Query retract(Term term)  {
		return query(new Compound(RETRACT, asList(term)));
	}
	
	@Override
	public boolean retractAll(Term term)  {
		return query(new Compound(RETRACT_ALL, asList(term))).hasSolution();
	}

	@Override
	public boolean abolish(Term term)  {
		return query(new Compound(ABOLISH, asList(term))).hasSolution();
	}
	
	@Override
	public Query clause(Term head, Term body)  {
		return query(new Compound(CLAUSE, asList(head, body)));
	}
	/**
	 * Assert a list of clauses in the logic database. Terms are asserted as the first facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean asserta(List<? extends Term> terms) {
		return allSucceed(LogicUtil.forEachApplyFunctor(ASSERTA, terms));
	}
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean assertz(List<? extends Term> terms) {
		return allSucceed(LogicUtil.forEachApplyFunctor(ASSERTZ, terms));
	}	
	
	
	/* ********************************************************************************************************************************
	 * FILE LOADER PREDICATES
     **********************************************************************************************************************************
     */
	@Override
	public boolean ensureLoaded(List<? extends Term> terms) {
		return query(new Compound(ENSURE_LOADED, asList(listTerm(terms)))).hasSolution();
	}
	
	@Override
	public boolean ensureLoaded(Term... terms) {
		return ensureLoaded(asList(terms));
	}

	@Override
	public boolean ensureLoaded(String... resources) {
		return query(new Compound(ENSURE_LOADED, asList(new Jpc().toTerm(resources)))).hasSolution();
	}
	

	/* ********************************************************************************************************************************
	 * HIGH ORDER PREDICATES
     **********************************************************************************************************************************
     */
	@Override
	public Query bagof(Term select, Term exp, Term all) {
		return query(new Compound(BAGOF, asList(select, exp, all)));
	}
	
	@Override
	public Term bagof(Term select, Term exp) {
		return bagof(select, exp, new Variable(ALL_RESULTS_VAR)).oneSolution().get(ALL_RESULTS_VAR);
	}
	
	@Override
	public Query findall(Term select, Term exp, Term all) {
		return query(new Compound(FINDALL, asList(select, exp, all)));
	}
	
	@Override
	public Term findall(Term select, Term exp) {
		return findall(select, exp, new Variable(ALL_RESULTS_VAR)).oneSolution().get(ALL_RESULTS_VAR);
	}
	
	@Override
	public Query setof(Term select, Term exp, Term all) {
		return query(new Compound(SETOF, asList(select, exp, all)));
	}
	
	@Override
	public Term setof(Term select, Term exp) {
		return setof(select, exp, new Variable(ALL_RESULTS_VAR)).oneSolution().get(ALL_RESULTS_VAR);
	}
	
	@Override
	public Query forall(Term generator, Term test) {
		return query(new Compound(FORALL, asList(generator, test)));
	}


	/* ********************************************************************************************************************************
	 * OTHER PREDICATES
     **********************************************************************************************************************************
     */
	@Override
	public boolean flushOutput() {
		return query(new Atom(FLUSH_OUTPUT)).hasSolution();
	}
	
	/* ********************************************************************************************************************************
	 * UNIFICATION
     **********************************************************************************************************************************
     */
	@Override
	public Term unify(Term... terms) {
		return unify(asList(terms));
	}
	
	@Override
	public Term unify(List<? extends Term> terms) {
		if(terms.isEmpty())
			throw new RuntimeException("The list of terms to unify cannot be empty");
		if(terms.size() == 1)
			return terms.get(0);
		List<Term> unifications = new ArrayList<>();
		for(int i=0; i<terms.size()-1; i++) {
			unifications.add(new Compound("=", asList(terms.get(i), terms.get(i+1))));
		}
		List<Map<String, Term>> solutions = query(termSequence(unifications)).allSolutions();
		if(solutions.isEmpty())
			return null;
		Map<String, Term> solution = solutions.get(0);
		return new QuerySolutionToTermFunction(terms.get(0)).apply(solution);
	}


	
	/* ********************************************************************************************************************************
	 * UTILITY METHODS
     **********************************************************************************************************************************
     */
	
	@Override
	public boolean allSucceed(List<? extends Term> terms) {
		boolean success = true;
		for(Term term: terms) {
			if(!query(term).hasSolution())
				success = false;
		}
		return success;
	}
	
}
