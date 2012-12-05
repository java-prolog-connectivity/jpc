package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ABOLISH;
import static org.jpc.engine.prolog.PrologConstants.ASSERTA;
import static org.jpc.engine.prolog.PrologConstants.ASSERTZ;
import static org.jpc.engine.prolog.PrologConstants.CLAUSE;
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
				((Compound)term).name().equals(LogtalkConstants.LOGTALK_OPERATOR) &&
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
	public Term asTerm() {
		return term;
	}

	public Term name() {
		if(term instanceof Compound)
			return ((Compound)term).name();
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
}
