package org.jpc.engine;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.jpc.flags.PrologFlag;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Query;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;


public abstract class BootstrapLogicEngine {

	public boolean halt() {
		throw new UnsupportedOperationException();
	}
	
	public boolean assertTerms(List<Term> terms) {
		return allSucceed(LogicUtil.forAllApplyFunctor("assert", terms));
	}

	public boolean ensureLoaded(List<Term> resourceTerms) {
		return allSucceed(LogicUtil.forAllApplyFunctor("ensure_loaded", resourceTerms));
	}
	
	public boolean allSucceed(List<Term> terms) {
		boolean success = true;
		for(Term term: terms) {
			if(!createQuery(term).hasSolution())
				success = false;
		}
		return success;
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
	
	public abstract Query createQuery(Term term);

	/**
	 * 
	 * @param termString
	 * @return the term representation of a String. Variable names should be preserved.
	 */
	public abstract Term asTerm(String termString);
	
	
	
}
