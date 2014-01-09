package org.jpc.engine.embedded.database;

import org.jpc.term.Term;

public interface IndexChangeListener {

	public void onIndexChange(UpdatableIndexFunction<Term, Object> updatableIndexFunction);
	
}
