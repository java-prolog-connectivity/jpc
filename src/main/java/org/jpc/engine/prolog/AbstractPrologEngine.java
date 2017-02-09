package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.*;
import static org.jpc.term.ListTerm.listTerm;
import static org.jpc.term.Var.ANONYMOUS_VAR;
import static org.jpc.util.JpcPreferences.JPC_LOADER_FILE;
import static org.jpc.util.JpcPreferences.JPC_LOGTALK_LOADER_FILE;
import static org.jpc.util.JpcPreferences.JPC_VAR_PREFIX;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.query.SolutionToTermFunction;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.expansion.PositionalSymbolExpander;
import org.jpc.util.LogtalkUtil;
import org.jpc.util.PrologUtil;
import org.jpc.util.TermJoiner;
import org.jpc.util.engine.PrologResourceLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.reflect.TypeToken;

public abstract class AbstractPrologEngine implements PrologEngine {

	private static final Logger logger = LoggerFactory.getLogger(AbstractPrologEngine.class);
	
	public static final String ALL_RESULTS_VAR = JPC_VAR_PREFIX + "ALL_RESULTS";
	
	public AbstractPrologEngine() {
	}
	
	/* ********************************************************************************************************************************
	 * CORE METHODS (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */

	@Override
	public abstract void close();

	@Override
	public boolean command(String command) {
		return query(command).hasSolution();
	}
	
	@Override
	public boolean command(String command, List<?> arguments) {
		return query(command, arguments).hasSolution();
	}
	
	@Override
	public boolean command(String command, Jpc context) {
		return query(command, context).hasSolution();
	}
	
	@Override
	public boolean command(String command, List<?> arguments, Jpc context) {
		return query(command, arguments, context).hasSolution();
	}
	
	@Override
	public final Query query(String goalString) {
		return query(goalString, Collections.emptyList());
	}
	
	@Override
	public final Query query(String goalString, List<?> arguments) {
		return query(goalString, arguments, JpcBuilder.create().build());
	}
	
	@Override
	public final Query query(String goalString, Jpc context) {
		return query(goalString, Collections.emptyList(), context);
	}
	
	@Override
	public final Query query(String goalString, List<?> arguments, Jpc context) {
		return query(goalString, arguments, true, context);
	}

	@Override
	public Query query(String goalString, List<?> arguments, boolean errorHandledQuery, Jpc context) {
		return query(asTerm(goalString, context), arguments, errorHandledQuery, context);
	}
 
	@Override
	public final Query query(Term goal) {
		return query(goal, JpcBuilder.create().build());
	}

	@Override
	public final Query query(Term goal, Jpc context) {
		return query(goal, Collections.emptyList(), true, context);
	}
	
	@Override
	public Query query(Term goal, List<?> arguments, boolean errorHandledQuery, Jpc context) {
		Term expandedGoal = goal.termExpansion(new PositionalSymbolExpander(arguments, context));
		Query query = basicQuery(expandedGoal, errorHandledQuery, context);
		return query;
	}
	
	@Override
	public Term asTerm(String termString) {
		return asTerm(termString, JpcBuilder.create().build());
	}
	
	@Override
	public List<Term> asTerms(List<String> termsString) {
		return asTerms(termsString, JpcBuilder.create().build());
	}
	
	@Override
	public List<Term> asTerms(List<String> termsString, Jpc context) {
		List<Term> terms = new ArrayList<>();
		for(String s : termsString)
			terms.add(asTerm(s, context));
		return terms;
	}

	@Override
	public Dialect dialect() {
		return Dialect.fromDialectFlag(dialectFlag());
	}


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
		Var varFlagValue = new Var("Var");
		Map<String, Term> solutions = currentPrologFlag(flag.asTerm(), varFlagValue).oneSolutionOrThrow();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlagValue.getName());
			flagValue = flagValueTerm.getName();
		}
		return flagValue;
	}
	
	@Override
	public String dialectFlag() {
		return currentPrologFlag(PrologFlag.DIALECT);
	}
	
	
	/* ********************************************************************************************************************************
	 * OPERATORS
     **********************************************************************************************************************************
     */
	
	@Override
	public OperatorsContext getOperatorsContext() {
		Term operatorsTerm = findall(ListTerm.create(new Var("P"), new Var("S"), new Var("O")).asTerm(), new Compound(CURRENT_OP, asList(new Var("P"), new Var("S"), new Var("O"))));
		return OperatorsContext.asOperatorsContext(operatorsTerm.asList());
	}
	
	
	@Override
	public Query currentOp(Term priority, Term specifier, Term operator) {
		return query(new Compound(CURRENT_OP, asList(priority, specifier, operator)));
	}

	@Override
	public boolean isBinaryOperator(String op) {
		TermJoiner joiner = TermJoiner.getDefault();
		return query(joiner.join(
				new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Var("Type"), new Atom(op))), 
				new Compound(ATOM_CHARS, Arrays.<Term>asList(new Var("Type"), listTerm(ANONYMOUS_VAR, new Atom("f"), ANONYMOUS_VAR)))
			)).hasSolution();
		//return createQuery("current_op(_, Type, '" + op + "'), atom_chars(Type, [_, f, _])").hasSolution();
	}
	
	@Override
	public boolean isUnaryOperator(String op) {
		TermJoiner joiner = TermJoiner.getDefault();
		return query(joiner.join(
				new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Var("Type"), 
				new Atom(op))), new Compound(ATOM_CHARS, asList(new Var("Type"), listTerm(new Atom("f"), ANONYMOUS_VAR)))
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
		return allSucceed(PrologUtil.forEachApplyFunctor(ASSERTA, terms));
	}
	
	/**
	 * Assert a list of clauses in the logic database. Term are asserted as the last facts or rules of the corresponding predicate.
	 * @param terms the terms to assert
	 * @return
	 */
	@Override
	public boolean assertz(List<? extends Term> terms) {
		return allSucceed(PrologUtil.forEachApplyFunctor(ASSERTZ, terms));
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
		Type targetType = new TypeToken<List<Atom>>(){}.getType();
		return ensureLoaded(JpcBuilder.create().build().<List<? extends Term>>convert(resources, targetType));
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
		return bagof(select, exp, new Var(ALL_RESULTS_VAR)).oneSolutionOrThrow().get(ALL_RESULTS_VAR);
	}
	
	@Override
	public Query findall(Term select, Term exp, Term all) {
		return query(new Compound(FINDALL, asList(select, exp, all)));
	}
	
	@Override
	public Term findall(Term select, Term exp) {
		return findall(select, exp, new Var(ALL_RESULTS_VAR)).oneSolutionOrThrow().get(ALL_RESULTS_VAR);
	}
	
	@Override
	public Query setof(Term select, Term exp, Term all) {
		return query(new Compound(SETOF, asList(select, exp, all)));
	}
	
	@Override
	public Term setof(Term select, Term exp) {
		return setof(select, exp, new Var(ALL_RESULTS_VAR)).oneSolutionOrThrow().get(ALL_RESULTS_VAR);
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
		TermJoiner joiner = TermJoiner.getDefault();
		List<Solution> solutions = query(joiner.join(unifications)).allSolutions();
		if(solutions.isEmpty())
			return null;
		Solution solution = solutions.get(0);
		return new SolutionToTermFunction(terms.get(0)).apply(solution);
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
	
	
	/* ********************************************************************************************************************************
	 * CONFIGURATION
     **********************************************************************************************************************************
     */
	
	@Override
	public final void loadJpc() {
		loadPrologFiles();
	}
	
	@Override
	public final void loadJpcForLogtalk() {
		loadPrologFiles();
		loadLogtalkFiles();
	}
	
	protected void loadPrologFiles() {
		PrologResourceLoader resourceLoader = new PrologResourceLoader(this);
		resourceLoader.ensureLoaded(JPC_LOADER_FILE);
	}
	
	protected void loadLogtalkFiles() {
		PrologResourceLoader resourceLoader = new PrologResourceLoader(this);
		resourceLoader.logtalkLoad(JPC_LOGTALK_LOADER_FILE);
	}
	
	/* ********************************************************************************************************************************
	 * Logtalk
     **********************************************************************************************************************************
     */
	
	@Override
	public LogtalkEngine asLogtalkEngine() {
		return new LogtalkEngine(this);
	}
	
	@Override
	public LogtalkEngine withLogtalk() {
		boolean logtalkLoaded = false;
		String prologDialect = dialectFlag();
		logger.info("Attempting to load logtalk in a " + prologDialect + " Prolog engine...");
		
		long startTime = System.nanoTime();
		try {
			String logtalkIntegrationScript = LogtalkUtil.logtalkIntegrationScriptOrThrow(prologDialect); //will throw an exception if a Logtalk integration script cannot be found for a given engine
			logtalkLoaded = ensureLoaded(logtalkIntegrationScript);
		} catch(Exception ex) {
			logger.error(ex.getMessage());
		}
		flushOutput();
		if(logtalkLoaded) {
			logger.trace("Logtalk loaded successfully.");
			loadJpcForLogtalk();
			logger.trace("Additional Logtalk configuration files were also loaded successfully.");
			long endTime = System.nanoTime();
			long total = (endTime - startTime)/1000000;
			logger.info("Logtalk was configured in " + prologDialect + " in " + total + " milliseconds.");
		}
		else {
			logger.warn("Impossible to load Logtalk in the " + prologDialect + " Logic Engine. Some features may not be available.");
		}
		return asLogtalkEngine();
	}
	
}
