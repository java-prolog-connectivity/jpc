package org.jpc.engine.logtalk;

import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_PREDICATE;
import static org.jpc.engine.logtalk.LogtalkConstants.PREDICATE_PROPERTY;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_CALL;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_EXIT;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_IGNORE;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_NOTIFY;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_ONCE;
import static org.jpc.engine.logtalk.LogtalkConstants.THREADED_PEEK;
import static org.jpc.engine.prolog.PrologConstants.ABOLISH;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.CLAUSE;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_OP;
import static org.jpc.engine.prolog.PrologConstants.RETRACT;
import static org.jpc.engine.prolog.PrologConstants.RETRACT_ALL;

import java.util.Arrays;

import org.jpc.Jpc;
import org.jpc.converter.TermConvertable;
import org.jpc.engine.prolog.PrologDatabase;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.LogtalkQuery;
import org.jpc.query.Query;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class LogtalkObject implements TermConvertable, PrologDatabase {

	public static Term logtalkMessage(Term receiver, Term message) {
		return new Compound(LogtalkConstants.LOGTALK_OPERATOR, Arrays.asList(receiver, message));
	}
	
	public static boolean isLogtalkMessage(Term term) {
		return term instanceof Compound && 
				((Compound)term).hasFunctor(LogtalkConstants.LOGTALK_OPERATOR, 2);
	}
	
	
	private PrologEngine prologEngine;
	private Term term;
	private Jpc context;
	
	public LogtalkObject(Object object, PrologEngine prologEngine) {
		this(object, prologEngine, new Jpc());
	}
	
	public LogtalkObject(Object object, PrologEngine prologEngine, Jpc context) {
		this.term = context.toTerm(object);
		this.prologEngine = prologEngine;
		this.context = context;
	}
	
	public Term message(Term message) {
		return logtalkMessage(this.asTerm(), message);
	}
	
	public Query perform(Term message) {
		return new LogtalkQuery(prologEngine.query(message(message), context));
	}
	
	@Override
	public String toString() {
		//return prologEngine.toString(asTerm()); //more accurate but less performant
		return asTerm().toString();
	}
	
	@Override
	public Term asTerm() {
		return term;
	}

	public Term name() {
		if(term instanceof Compound)
			return ((Compound)term).getName();
		else
			return term;
	}
	
	public int arity() {
		return term.arity();
	}
	
	@Override
	public boolean asserta(Term term) {
		return perform(new Compound(ASSERTA, Arrays.asList(term))).hasSolution();
	}

	@Override
	public boolean assertz(Term term) {
		return perform(new Compound(ASSERTZ, Arrays.asList(term))).hasSolution();
	}

	@Override
	public Query retract(Term term) {
		return perform(new Compound(RETRACT, Arrays.asList(term)));
	}

	@Override
	public boolean retractAll(Term term) {
		return perform(new Compound(RETRACT_ALL, Arrays.asList(term))).hasSolution();
	}

	@Override
	public boolean abolish(Term term) {
		return perform(new Compound(ABOLISH, Arrays.asList(term))).hasSolution();
	}
	
	public Query clause(Term head, Term body)  {
		return perform(new Compound(CLAUSE, Arrays.asList(head, body)));
	}
	
	public Query currentOp(Term priority, Term specifier, Term operator) {
		return perform(new Compound(CURRENT_OP, Arrays.asList(priority, specifier, operator)));
	}
	
	public Query predicateProperty(Term predicate, Term property) {
		return perform(new Compound(PREDICATE_PROPERTY, Arrays.asList(predicate, property)));
	}
	
	public Query currentPredicate(Term predicate) {
		return perform(new Compound(CURRENT_PREDICATE, Arrays.asList(predicate)));
	}
	
	public Query threaded(Term goals) {
		return perform(new Compound(THREADED, Arrays.asList(goals)));
	}
	
	public boolean threadedCall(Term goal) {
		return perform(new Compound(THREADED_CALL, Arrays.asList(goal))).hasSolution();
	}
	
	public boolean threadedCall(Term goal, Term tag) {
		return perform(new Compound(THREADED_CALL, Arrays.asList(goal, tag))).hasSolution();
	}
	
	public boolean threadedOnce(Term goal) {
		return perform(new Compound(THREADED_ONCE, Arrays.asList(goal))).hasSolution();
	}
	
	public boolean threadedOnce(Term goal, Term tag) {
		return perform(new Compound(THREADED_ONCE, Arrays.asList(goal, tag))).hasSolution();
	}
	
	public boolean threadedPeek(Term goal) {
		return perform(new Compound(THREADED_PEEK, Arrays.asList(goal))).hasSolution();
	}
	
	public boolean threadedPeek(Term goal, Term tag) {
		return perform(new Compound(THREADED_PEEK, Arrays.asList(goal, tag))).hasSolution();
	}
	
	public Query threadedExit(Term goal) {
		return perform(new Compound(THREADED_EXIT, Arrays.asList(goal)));
	}
	
	public Query threadedExit(Term goal, Term tag) {
		return perform(new Compound(THREADED_EXIT, Arrays.asList(goal, tag)));
	}
	
	public boolean threadedIgnore(Term goal) {
		return perform(new Compound(THREADED_IGNORE, Arrays.asList(goal))).hasSolution();
	}

	public Query threadedNotify(Term term) {
		return perform(new Compound(THREADED_NOTIFY, Arrays.asList(term)));
	}
	
}
