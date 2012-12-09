package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;
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

import org.jpc.engine.prolog.DatabaseHandler;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.Query;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public class LogtalkObject implements TermConvertable, DatabaseHandler {

	public static boolean isLogtalkMessage(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		return term instanceof Compound && 
				((Compound)term).getName().equals(LogtalkConstants.LOGTALK_OPERATOR) &&
				((Compound)term).arity() == 2;
	}
	
	private PrologEngine logicEngine;
	private Term term;
	
	public LogtalkObject(PrologEngine logicEngine, TermConvertable termConvertable) {
		this.logicEngine = logicEngine;
		this.term = termConvertable.asTerm();
	}
	
	private Term messageTerm(TermConvertable message) {
		return new Compound(LogtalkConstants.LOGTALK_OPERATOR, asList(this, message));
	}
	
	public Query perform(TermConvertable message) {
		return logicEngine.createQuery(messageTerm(message));
	}
	
	@Override
	public String toString() {
		//return logicEngine.toString(asTerm()); //more accurate but less performant
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
	public boolean asserta(TermConvertable termConvertable) {
		return perform(new Compound(ASSERTA, asList(termConvertable))).hasSolution();
	}

	@Override
	public boolean assertz(TermConvertable termConvertable) {
		return perform(new Compound(ASSERTZ, asList(termConvertable))).hasSolution();
	}

	@Override
	public Query retract(TermConvertable termConvertable) {
		return perform(new Compound(RETRACT, asList(termConvertable)));
	}

	@Override
	public boolean retractAll(TermConvertable termConvertable) {
		return perform(new Compound(RETRACT_ALL, asList(termConvertable))).hasSolution();
	}

	@Override
	public boolean abolish(TermConvertable termConvertable) {
		return perform(new Compound(ABOLISH, asList(termConvertable))).hasSolution();
	}
	
	public Query clause(TermConvertable head, TermConvertable body)  {
		return perform(new Compound(CLAUSE, asList(head, body)));
	}
	
	public Query currentOp(TermConvertable priority, TermConvertable specifier, TermConvertable operator) {
		return perform(new Compound(CURRENT_OP, asList(priority, specifier, operator)));
	}
	
	public Query predicateProperty(TermConvertable predicate, TermConvertable property) {
		return perform(new Compound(PREDICATE_PROPERTY, asList(predicate, property)));
	}
	
	public Query currentPredicate(TermConvertable predicate) {
		return perform(new Compound(CURRENT_PREDICATE, asList(predicate)));
	}
	
	public Query threaded(TermConvertable goals) {
		return perform(new Compound(THREADED, asList(goals)));
	}
	
	public boolean threadedCall(TermConvertable goal) {
		return perform(new Compound(THREADED_CALL, asList(goal))).hasSolution();
	}
	
	public boolean threadedCall(TermConvertable goal, TermConvertable tag) {
		return perform(new Compound(THREADED_CALL, asList(goal, tag))).hasSolution();
	}
	
	public boolean threadedOnce(TermConvertable goal) {
		return perform(new Compound(THREADED_ONCE, asList(goal))).hasSolution();
	}
	
	public boolean threadedOnce(TermConvertable goal, TermConvertable tag) {
		return perform(new Compound(THREADED_ONCE, asList(goal, tag))).hasSolution();
	}
	
	public boolean threadedPeek(TermConvertable goal) {
		return perform(new Compound(THREADED_PEEK, asList(goal))).hasSolution();
	}
	
	public boolean threadedPeek(TermConvertable goal, TermConvertable tag) {
		return perform(new Compound(THREADED_PEEK, asList(goal, tag))).hasSolution();
	}
	
	public Query threadedExit(TermConvertable goal) {
		return perform(new Compound(THREADED_EXIT, asList(goal)));
	}
	
	public Query threadedExit(TermConvertable goal, TermConvertable tag) {
		return perform(new Compound(THREADED_EXIT, asList(goal, tag)));
	}
	
	public boolean threadedIgnore(TermConvertable goal) {
		return perform(new Compound(THREADED_IGNORE, asList(goal))).hasSolution();
	}

	public Query threadedNotify(TermConvertable termConvertable) {
		return perform(new Compound(THREADED_NOTIFY, asList(termConvertable)));
	}
	
}
