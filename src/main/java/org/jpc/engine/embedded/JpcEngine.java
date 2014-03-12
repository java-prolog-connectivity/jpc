package org.jpc.engine.embedded;

import static org.jpc.engine.prolog.ThreadModel.MULTI_THREADED;

import org.jpc.Jpc;
import org.jpc.engine.embedded.database.ClauseDatabase;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.engine.prolog.AbstractPrologEngine;
import org.jpc.engine.prolog.ThreadModel;
import org.jpc.query.Query;
import org.jpc.term.Term;

/**
 * The embedded Jpc Prolog database.
 * (Note: only a small subset of a Prolog engine has been currently implemented).
 * @author sergioc
 *
 */
public class JpcEngine extends AbstractPrologEngine {

	//private final ClauseDatabase systemDatabase; //TODO make use of a system database for storing directives and so on...
	private final ClauseDatabase userDatabase;
	private final MutableIndexManager userIndexManager;

	public JpcEngine() {
		this(new MutableIndexManager());
	}

	private JpcEngine(MutableIndexManager indexManager) {
		this.userIndexManager = indexManager;
		userDatabase = new ClauseDatabase(indexManager);
		//systemDatabase = new ClauseDatabase(IndexManager.getSystemIndexManager()); 
	}
	
	public MutableIndexManager getIndexManager() {
		return userIndexManager;
	}
	
	public ClauseDatabase getClauseDatabase() {
		return userDatabase;
	}
	
	@Override
	public boolean isCloseable() {
		return true;
	}

	@Override
	public ThreadModel threadModel() {
		return MULTI_THREADED;
	}

	@Override
	public Query basicQuery(Term goal, boolean errorHandledQuery, Jpc context) {
		return new JpcQuery(this, goal, errorHandledQuery, context);
	}

	@Override
	public Term asTerm(String termString, Jpc context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void close() {
		userDatabase.reset();
	}
	
	
	
	//TODO The methods below should be replaced by a better implementation. They are fine just for prototyping and testing.
	
	@Override
	public boolean asserta(Term term) {
		userDatabase.asserta(term);
		return true;
	}
	
	@Override
	public boolean assertz(Term term) {
		userDatabase.assertz(term);
		return true;
	}

	public boolean retractOne(Term term)  {
		return userDatabase.retract(term);
	}
	
	@Override
	public boolean retractAll(Term term)  {
		userDatabase.retractAll(term);
		return true;
	}
	

}
