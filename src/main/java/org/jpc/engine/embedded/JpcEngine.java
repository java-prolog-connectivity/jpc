package org.jpc.engine.embedded;

import org.jpc.Jpc;
import org.jpc.engine.embedded.database.ClauseDatabase;
import org.jpc.engine.embedded.database.IndexManager;
import org.jpc.engine.prolog.AbstractPrologEngine;
import org.jpc.query.Query;
import org.jpc.term.Term;

/**
 * The embedded Jpc Prolog engine.
 * (Note: just a small subset of a Prolog engine has been implemented for testing and prototyping purposes).
 * @author sergioc
 *
 */
public class JpcEngine extends AbstractPrologEngine {

	private final ClauseDatabase clauseDatabase;
	private final IndexManager indexManager;
	
	public JpcEngine() {
		this(new IndexManager());
	}
	
	private JpcEngine(IndexManager indexManager) {
		this.indexManager = indexManager;
		clauseDatabase = new ClauseDatabase(indexManager);
	}
	
	public IndexManager getIndexManager() {
		return indexManager;
	}
	
	public ClauseDatabase getClauseDatabase() {
		return clauseDatabase;
	}
	
	@Override
	public boolean isCloseable() {
		return true;
	}

	@Override
	public boolean isMultiThreaded() {
		return true;
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
		clauseDatabase.reset();
	}
	
	
	
	//TODO The methods below should be replaced by a better implementation. They are fine just for prototyping and testing.
	
	@Override
	public boolean assertz(Term term) {
		clauseDatabase.assertz(term);
		return true;
	}

	public boolean retractOne(Term term)  {
		return clauseDatabase.retract(term);
	}
	
	@Override
	public boolean retractAll(Term term)  {
		clauseDatabase.retractAll(term);
		return true;
	}
	

}
