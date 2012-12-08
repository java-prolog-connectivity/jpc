package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.ATOM_CHARS;
import static org.jpc.engine.prolog.PrologConstants.BAGOF;
import static org.jpc.engine.prolog.PrologConstants.CD;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_OP;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_PROLOG_FLAG;
import static org.jpc.engine.prolog.PrologConstants.FINDALL;
import static org.jpc.engine.prolog.PrologConstants.FORALL;
import static org.jpc.engine.prolog.PrologConstants.SETOF;
import static org.jpc.engine.prolog.PrologConstants.SET_PROLOG_FLAG;
import static org.jpc.term.ListTerm.listTerm;
import static org.jpc.term.Variable.ANONYMOUS_VAR;
import static org.jpc.util.LogicUtil.isResourceAlias;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;
import org.jpc.util.salt.TermToStringBuilder;

/**
 * A utility class for interacting with a Prolog engine
 * Extends the BootstrapLogicEngine by composition
 * It also adds new functionality
 * @author sergioc
 *
 */
public class PrologEngine implements DatabaseHandler {

	private BootstrapPrologEngine bootstrapEngine;

	public PrologEngine(BootstrapPrologEngine bootstrapEngine) {
		this.bootstrapEngine = bootstrapEngine;
	}
	
	public BootstrapPrologEngine getBootstrapEngine() {
		return bootstrapEngine;
	}
	
	public LogtalkEngine asLogtalkEngine() {
		return new LogtalkEngine(bootstrapEngine);
	}
	
	
	
	/* ********************************************************************************************************************************
	 * PROXY METHODS IMPLEMENTED IN THE BOOTSTRAP ENGINE (and overloaded variations of those methods)
     **********************************************************************************************************************************
     */
	
	public Query createQuery(TermConvertable... termConvertables) {
		Term termSequence = LogicUtil.termsToSequence(asList(termConvertables));
		return bootstrapEngine.createQuery(termSequence);
	}
	
	public Query createQuery(String termString) {
		return createQuery(asTerm(termString));
	}
	
	public boolean stop() {
		return bootstrapEngine.stop();
	}
	
	public Term asTerm(String termString) {
		return asTerm(termString, false);
	}
	
	public Term asTerm(String termString, boolean force) {
		try {
			return bootstrapEngine.asTerm(termString);
		} catch(Exception e) {
			if(force)
				return new Atom(termString);
			else
				throw e;
		}
	}
	
	public List<Term> asTerms(List<String> termsString) {
		return asTerms(termsString, false);
	}
	
	public List<Term> asTerms(List<String> termsString, boolean force) {
		List<Term> terms = new ArrayList<>();
		for(String s : termsString)
			terms.add(asTerm(s, force));
		return terms;
	}
	
	public String escape(String s) {
		return bootstrapEngine.escape(s);
	}
	
	
	
	
	
	

	


	



	

	

	
	
	
	
	
	
	public boolean flushOutput() {
		return bootstrapEngine.flushOutput();
	}
	
	public boolean setPrologFlag(TermConvertable flag, TermConvertable flagValue) {
		return bootstrapEngine.setPrologFlag(flag, flagValue);
	}
	
	public boolean setPrologFlag(TermConvertable flag, String flagValue) {
		return setPrologFlag(flag, new Atom(flagValue));
	}
	
	public Query currentPrologFlag(TermConvertable flag, TermConvertable flagValue) {
		return bootstrapEngine.currentPrologFlag(flag, flagValue);
	}
	
	public String currentPrologFlag(TermConvertable flag) {
		String flagValue = null;
		Variable varFlagValue = new Variable("Var");
		Map<String, Term> solutions = currentPrologFlag(flag, varFlagValue).oneSolution();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlagValue.name());
			flagValue = flagValueTerm.getName();
		}
		return flagValue;
	}
	
	public String prologDialect() {
		return currentPrologFlag(PrologFlag.DIALECT);
	}
	
	public Query currentOp(TermConvertable priority, TermConvertable specifier, TermConvertable operator) {
		return bootstrapEngine.currentOp(priority, specifier, operator);
	}

	public boolean isBinaryOperator(String op) {
		return createQuery(new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Variable("Type"), new Atom(op))), new Compound(ATOM_CHARS, asList(new Variable("Type"), listTerm(ANONYMOUS_VAR, new Atom("f"), ANONYMOUS_VAR)))).hasSolution();
		//return createQuery("current_op(_, Type, '" + op + "'), atom_chars(Type, [_, f, _])").hasSolution();
	}
	
	public boolean isUnaryOperator(String op) {
		return createQuery(new Compound(CURRENT_OP, asList(ANONYMOUS_VAR, new Variable("Type"), new Atom(op))), new Compound(ATOM_CHARS, asList(new Variable("Type"), listTerm(new Atom("f"), ANONYMOUS_VAR)))).hasSolution();
		//return createQuery("current_op(_, Type, '" + op + "'), atom_chars(Type, [f, _])").hasSolution();
	}
	
	
	
	
	
	@Override
	public boolean asserta(TermConvertable termConvertable) {
		return bootstrapEngine.asserta(termConvertable);
	}
	
	@Override
	public boolean assertz(TermConvertable termConvertable) {
		return bootstrapEngine.assertz(termConvertable);
	}

	@Override
	public Query retract(TermConvertable termConvertable) {
		return bootstrapEngine.retract(termConvertable);
	}
	
	@Override
	public boolean retractAll(TermConvertable termConvertable) {
		return bootstrapEngine.retractAll(termConvertable);
	}
	
	@Override
	public boolean abolish(TermConvertable termConvertable) {
		return bootstrapEngine.abolish(termConvertable);
	}
	
	@Override
	public Query clause(TermConvertable head, TermConvertable body) {
		return bootstrapEngine.clause(head, body);
	}
	
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
	
	
	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables) {
		return bootstrapEngine.ensureLoaded(termConvertables);
	}
	
	public boolean ensureLoaded(TermConvertable... termConvertables) {
		return bootstrapEngine.ensureLoaded(asList(termConvertables));
	}

	public boolean ensureLoaded(String... resources) {
		return bootstrapEngine.ensureLoaded(asResourceTerms(asList(resources)));
	}
	
	public boolean cd(TermConvertable path) {
		return bootstrapEngine.cd(path);
	}
	
	public boolean cd(String path) {
		return cd(new Atom(path));
	}
	
	public Query bagof(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return bootstrapEngine.bagof(select, exp, all);
	}
	
	public Query findall(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return bootstrapEngine.findall(select, exp, all);
	}
	
	public Query setof(TermConvertable select, TermConvertable exp, TermConvertable all) {
		return bootstrapEngine.setof(select, exp, all);
	}
	
	public Query forall(TermConvertable generator, TermConvertable test) {
		return bootstrapEngine.forall(generator, test);
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
	
	public List<Term> asResourceTerms(List<String> resourceNames) {
		List<Term> terms = new ArrayList<>();
		for(String s : resourceNames)
			terms.add(asResourceTerm(s));
		return terms;
	}
	
	public Term asResourceTerm(String resourceName) {
		if(isResourceAlias(resourceName)) //it is a resource alias of the form library(lib_name)
			return asTerm(resourceName);
		else
			return new Atom(resourceName);
	}
	
	public String toString(TermConvertable termConvertable) {
		TermToStringBuilder termToStringBuilder = new TermToStringBuilder(this);
		termConvertable.asTerm().read(termToStringBuilder);
		return termToStringBuilder.toString();
	}
	
	public String termSequenceToString(TermConvertable sequenceTermConvertable) {
		List<Term> terms = LogicUtil.sequenceAsTerms(sequenceTermConvertable);
		String sequenceString = "";
		for(int i = 0; i<terms.size(); i++) {
			sequenceString += toString(terms.get(i));
			if(i<terms.size()-1)
				sequenceString += ", ";
		}
		return sequenceString;
	}

}
