package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.logtalk.LogtalkConstants.ABOLISH_OBJECT;
import static org.jpc.engine.prolog.PrologConstants.CD;
import static org.jpc.engine.logtalk.LogtalkConstants.CREATE_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.LOGTALK_LOAD;
import static org.jpc.engine.logtalk.LogtalkConstants.SET_LOGTALK_FLAG;
import static org.jpc.util.LogicUtil.forEachApplyFunctor;
import static org.jpc.util.LogicUtil.isResourceAlias;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.engine.logtalk.LogtalkFlag;
import org.jpc.engine.logtalk.LogtalkObject;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;
import org.jpc.util.salt.TermToStringBuilder;

/**
 * Extends the BootstrapLogicEngine by composition
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
	
	public boolean stop() {
		return bootstrapEngine.stop();
	}
	
	public Query createQuery(TermConvertable termConvertable) {
		return bootstrapEngine.createQuery(termConvertable);
	}
	
	public Query createQuery(String termString) {
		return createQuery(asTerm(termString));
	}
	
	public String toString(Term term) {
		TermToStringBuilder termToStringBuilder = new TermToStringBuilder(this);
		term.read(termToStringBuilder);
		return termToStringBuilder.toString();
	}
	
	public String escape(String s) {
		return bootstrapEngine.escape(s);
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
	
	public boolean asserta(List<? extends TermConvertable> termConvertables) {
		return bootstrapEngine.asserta(termConvertables);
	}
	
	public boolean assertz(List<? extends TermConvertable> termConvertables) {
		return bootstrapEngine.assertz(termConvertables);
	}
	
	
	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables) {
		return bootstrapEngine.ensureLoaded(termConvertables);
	}
	
	public boolean ensureLoaded(TermConvertable... termConvertables) {
		return bootstrapEngine.ensureLoaded(termConvertables);
	}

	public boolean ensureLoaded(String... resources) {
		return bootstrapEngine.ensureLoaded(asResourceTerms(asList(resources)));
	}

	public boolean allSucceed(List<? extends TermConvertable> termConvertables) {
		return bootstrapEngine.allSucceed(termConvertables);
	}
	
	
	
	public String currentPrologFlag(String flagName) {
		return bootstrapEngine.currentPrologFlag(flagName);
	}
	
	public String prologDialect() {
		return bootstrapEngine.prologDialect();
	}
	
	public boolean cd(String path) {
		Compound compound = new Compound(CD, asList(new Atom(path)));
		return createQuery(compound).hasSolution();
	}

	
	/**
	 * Answers an array of anonymous logic variables
	 * @param n the number of variables in the array
	 * @return
	 */
	public static List<Variable> anonymousVariables(int n) {
		List<Variable> variablesList = new ArrayList<>();
		for(int i=0; i<n; i++) {
			variablesList.add(Variable.ANONYMOUS_VAR);
		}
		return variablesList;
	}
	


	public boolean isBinaryOperator(String op) {
		return createQuery("Op='" + op + "', current_op(_, Type, Op), atom_chars(Type, Chars), Chars=[_, f, _]").hasSolution();
	}
	
	public boolean isUnaryOperator(String op) {
		return createQuery("Op='" + op + "', current_op(_, Type, Op), atom_chars(Type, Chars), Chars=[f, _]").hasSolution();
	}
	
	public boolean flushOutput() {
		return bootstrapEngine.flushOutput();
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
