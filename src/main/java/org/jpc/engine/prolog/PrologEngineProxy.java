package org.jpc.engine.prolog;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.query.Query;
import org.jpc.term.Term;

public class PrologEngineProxy implements PrologEngine {

	private PrologEngine proxiedEngine;
	
	public PrologEngineProxy(PrologEngine proxiedEngine) {
		this.proxiedEngine = proxiedEngine;
	}
	
	/**
	 * To allows descendants to instantiate the proxied Prolog engines in a custom way
	 */
	protected PrologEngineProxy() {
	}
	
	protected void setPrologEngine(PrologEngine proxiedEngine) {
		this.proxiedEngine = proxiedEngine;
	}
	
	public PrologEngine getPrologEngine() {
		return proxiedEngine;
	}

	public LogtalkEngine asLogtalkEngine() {
		return proxiedEngine.asLogtalkEngine();
	}
	
	@Override
	public void close() {
		proxiedEngine.close();
	}
	
	@Override
	public boolean isCloseable() {
		return proxiedEngine.isCloseable();
	}

	@Override
	public boolean isMultiThreaded() {
		return proxiedEngine.isMultiThreaded();
	}
	
	@Override
	public boolean command(String termString) {
		return proxiedEngine.command(termString);
	}

	@Override
	public boolean command(String termString, boolean errorHandledQuery) {
		return proxiedEngine.command(termString, errorHandledQuery);
	}
	
	@Override
	public boolean command(String termString, Jpc context) {
		return proxiedEngine.command(termString, context);
	}
	
	@Override
	public Query query(String termString) {
		return proxiedEngine.query(termString);
	}

	@Override
	public Query query(Term term) {
		return proxiedEngine.query(term);
	}
	
	@Override
	public Query query(String termString, boolean errorHandledQuery) {
		return proxiedEngine.query(termString, errorHandledQuery);
	}
	
	@Override
	public Query query(Term term, boolean errorHandledQuery) {
		return proxiedEngine.query(term, errorHandledQuery);
	}
	
	@Override
	public Query query(String termString, Jpc context) {
		return proxiedEngine.query(termString, context);
	}

	@Override
	public Query query(Term term, Jpc context) {
		return proxiedEngine.query(term, context);
	}
	
	@Override
	public Query query(String termString, boolean errorHandledQuery, Jpc context) {
		return proxiedEngine.query(termString, errorHandledQuery, context);
	}
	
	@Override
	public Query query(Term term, boolean errorHandledQuery, Jpc context) {
		return proxiedEngine.query(term, errorHandledQuery, context);
	}
	
	@Override
	public Term asTerm(String termString) {
		return proxiedEngine.asTerm(termString);
	}

//	public Term asTerm(String termString, boolean force) {
//		return prologEngine.asTerm(termString, force);
//	}

	@Override
	public List<Term> asTerms(List<String> termsString) {
		return proxiedEngine.asTerms(termsString);
	}

//	public List<Term> asTerms(List<String> termsString, boolean force) {
//		return prologEngine.asTerms(termsString, force);
//	}

	@Override
	public boolean setPrologFlag(Term flag, Term flagValue) {
		return proxiedEngine.setPrologFlag(flag, flagValue);
	}

	@Override
	public boolean setPrologFlag(Flag flag, String flagValue) {
		return proxiedEngine.setPrologFlag(flag, flagValue);
	}

	@Override
	public Query currentPrologFlag(Term flag, Term flagValue) {
		return proxiedEngine.currentPrologFlag(flag, flagValue);
	}

	@Override
	public Query currentPrologFlag(Flag flag, String flagValue) {
		return proxiedEngine.currentPrologFlag(flag, flagValue);
	}
	
	@Override
	public String currentPrologFlag(Flag flag) {
		return proxiedEngine.currentPrologFlag(flag);
	}

	@Override
	public String prologDialect() {
		return proxiedEngine.prologDialect();
	}

	@Override
	public Query currentOp(Term priority, Term specifier, Term operator) {
		return proxiedEngine.currentOp(priority, specifier, operator);
	}

	@Override
	public boolean isBinaryOperator(String op) {
		return proxiedEngine.isBinaryOperator(op);
	}

	@Override
	public boolean isUnaryOperator(String op) {
		return proxiedEngine.isUnaryOperator(op);
	}

	@Override
	public boolean cd(Term path) {
		return proxiedEngine.cd(path);
	}

	@Override
	public boolean cd(String path) {
		return proxiedEngine.cd(path);
	}

	@Override
	public boolean asserta(Term term) {
		return proxiedEngine.asserta(term);
	}

	@Override
	public boolean assertz(Term term) {
		return proxiedEngine.assertz(term);
	}

	@Override
	public Query retract(Term term) {
		return proxiedEngine.retract(term);
	}

	@Override
	public boolean retractAll(Term term) {
		return proxiedEngine.retractAll(term);
	}

	@Override
	public boolean abolish(Term term) {
		return proxiedEngine.abolish(term);
	}

	@Override
	public Query clause(Term head, Term body) {
		return proxiedEngine.clause(head, body);
	}

	@Override
	public boolean asserta(List<? extends Term> terms) {
		return proxiedEngine.asserta(terms);
	}

	@Override
	public boolean assertz(List<? extends Term> terms) {
		return proxiedEngine.assertz(terms);
	}

	@Override
	public boolean ensureLoaded(List<? extends Term> terms) {
		return proxiedEngine.ensureLoaded(terms);
	}

	@Override
	public boolean ensureLoaded(Term... terms) {
		return proxiedEngine.ensureLoaded(terms);
	}

	@Override
	public boolean ensureLoaded(String... resources) {
		return proxiedEngine.ensureLoaded(resources);
	}

	@Override
	public Query bagof(Term select, Term exp, Term all) {
		return proxiedEngine.bagof(select, exp, all);
	}

	@Override
	public Term bagof(Term select, Term exp) {
		return proxiedEngine.bagof(select, exp);
	}
	
	@Override
	public Query findall(Term select, Term exp, Term all) {
		return proxiedEngine.findall(select, exp, all);
	}

	@Override
	public Term findall(Term select, Term exp) {
		return proxiedEngine.findall(select, exp);
	}
	
	@Override
	public Query setof(Term select, Term exp, Term all) {
		return proxiedEngine.setof(select, exp, all);
	}

	@Override
	public Term setof(Term select, Term exp) {
		return proxiedEngine.setof(select, exp);
	}
	
	@Override
	public Query forall(Term generator, Term test) {
		return proxiedEngine.forall(generator, test);
	}

	@Override
	public boolean flushOutput() {
		return proxiedEngine.flushOutput();
	}

	@Override
	public Term unify(Term... terms) {
		return proxiedEngine.unify(terms);
	}
	
	@Override
	public Term unify(List<? extends Term> terms) {
		return proxiedEngine.unify(terms);
	}

	@Override
	public boolean allSucceed(List<? extends Term> Terms) {
		return proxiedEngine.allSucceed(Terms);
	}
}
