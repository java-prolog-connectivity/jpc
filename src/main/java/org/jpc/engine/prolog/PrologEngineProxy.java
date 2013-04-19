package org.jpc.engine.prolog;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.query.Query;
import org.jpc.term.Term;

public class PrologEngineProxy implements PrologEngine {

	private PrologEngine prologEngine;
	
	public PrologEngineProxy(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}
	
	/**
	 * To allows descendants to instantiate the proxied Prolog engines in a custom way
	 */
	protected PrologEngineProxy() {
	}
	
	protected void setPrologEngine(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}
	
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}

	public LogtalkEngine asLogtalkEngine() {
		return prologEngine.asLogtalkEngine();
	}
	
	@Override
	public void close() {
		prologEngine.close();
	}

	@Override
	public boolean isCloseable() {
		return prologEngine.isCloseable();
	}
	
	@Override
	public boolean command(String termString) {
		return prologEngine.command(termString);
	}

	@Override
	public boolean command(String termString, boolean errorHandledQuery) {
		return prologEngine.command(termString, errorHandledQuery);
	}
	
	@Override
	public boolean command(String termString, Jpc context) {
		return prologEngine.command(termString, context);
	}
	
	@Override
	public Query query(String termString) {
		return prologEngine.query(termString);
	}

	@Override
	public Query query(Term term) {
		return prologEngine.query(term);
	}
	
	@Override
	public Query query(String termString, boolean errorHandledQuery) {
		return prologEngine.query(termString, errorHandledQuery);
	}
	
	@Override
	public Query query(Term term, boolean errorHandledQuery) {
		return prologEngine.query(term, errorHandledQuery);
	}
	
	@Override
	public Query query(String termString, Jpc context) {
		return prologEngine.query(termString, context);
	}

	@Override
	public Query query(Term term, Jpc context) {
		return prologEngine.query(term, context);
	}
	
	@Override
	public Query query(String termString, boolean errorHandledQuery, Jpc context) {
		return prologEngine.query(termString, errorHandledQuery, context);
	}
	
	@Override
	public Query query(Term term, boolean errorHandledQuery, Jpc context) {
		return prologEngine.query(term, errorHandledQuery, context);
	}
	
	@Override
	public Term asTerm(String termString) {
		return prologEngine.asTerm(termString);
	}

//	public Term asTerm(String termString, boolean force) {
//		return prologEngine.asTerm(termString, force);
//	}

	@Override
	public List<Term> asTerms(List<String> termsString) {
		return prologEngine.asTerms(termsString);
	}

//	public List<Term> asTerms(List<String> termsString, boolean force) {
//		return prologEngine.asTerms(termsString, force);
//	}

	@Override
	public boolean setPrologFlag(Term flag, Term flagValue) {
		return prologEngine.setPrologFlag(flag, flagValue);
	}

	@Override
	public boolean setPrologFlag(Flag flag, String flagValue) {
		return prologEngine.setPrologFlag(flag, flagValue);
	}

	@Override
	public Query currentPrologFlag(Term flag, Term flagValue) {
		return prologEngine.currentPrologFlag(flag, flagValue);
	}

	@Override
	public Query currentPrologFlag(Flag flag, String flagValue) {
		return prologEngine.currentPrologFlag(flag, flagValue);
	}
	
	@Override
	public String currentPrologFlag(Flag flag) {
		return prologEngine.currentPrologFlag(flag);
	}

	@Override
	public String prologDialect() {
		return prologEngine.prologDialect();
	}

	@Override
	public Query currentOp(Term priority, Term specifier, Term operator) {
		return prologEngine.currentOp(priority, specifier, operator);
	}

	@Override
	public boolean isBinaryOperator(String op) {
		return prologEngine.isBinaryOperator(op);
	}

	@Override
	public boolean isUnaryOperator(String op) {
		return prologEngine.isUnaryOperator(op);
	}

	@Override
	public boolean cd(Term path) {
		return prologEngine.cd(path);
	}

	@Override
	public boolean cd(String path) {
		return prologEngine.cd(path);
	}

	@Override
	public boolean asserta(Term term) {
		return prologEngine.asserta(term);
	}

	@Override
	public boolean assertz(Term term) {
		return prologEngine.assertz(term);
	}

	@Override
	public Query retract(Term term) {
		return prologEngine.retract(term);
	}

	@Override
	public boolean retractAll(Term term) {
		return prologEngine.retractAll(term);
	}

	@Override
	public boolean abolish(Term term) {
		return prologEngine.abolish(term);
	}

	@Override
	public Query clause(Term head, Term body) {
		return prologEngine.clause(head, body);
	}

	@Override
	public boolean asserta(List<? extends Term> terms) {
		return prologEngine.asserta(terms);
	}

	@Override
	public boolean assertz(List<? extends Term> terms) {
		return prologEngine.assertz(terms);
	}

	@Override
	public boolean ensureLoaded(List<? extends Term> terms) {
		return prologEngine.ensureLoaded(terms);
	}

	@Override
	public boolean ensureLoaded(Term... terms) {
		return prologEngine.ensureLoaded(terms);
	}

	@Override
	public boolean ensureLoaded(String... resources) {
		return prologEngine.ensureLoaded(resources);
	}

	@Override
	public Query bagof(Term select, Term exp, Term all) {
		return prologEngine.bagof(select, exp, all);
	}

	@Override
	public Term bagof(Term select, Term exp) {
		return prologEngine.bagof(select, exp);
	}
	
	@Override
	public Query findall(Term select, Term exp, Term all) {
		return prologEngine.findall(select, exp, all);
	}

	@Override
	public Term findall(Term select, Term exp) {
		return prologEngine.findall(select, exp);
	}
	
	@Override
	public Query setof(Term select, Term exp, Term all) {
		return prologEngine.setof(select, exp, all);
	}

	@Override
	public Term setof(Term select, Term exp) {
		return prologEngine.setof(select, exp);
	}
	
	@Override
	public Query forall(Term generator, Term test) {
		return prologEngine.forall(generator, test);
	}

	@Override
	public boolean flushOutput() {
		return prologEngine.flushOutput();
	}

	@Override
	public Term unify(Term... terms) {
		return prologEngine.unify(terms);
	}
	
	@Override
	public Term unify(List<? extends Term> terms) {
		return prologEngine.unify(terms);
	}

	@Override
	public boolean allSucceed(List<? extends Term> Terms) {
		return prologEngine.allSucceed(Terms);
	}
}
