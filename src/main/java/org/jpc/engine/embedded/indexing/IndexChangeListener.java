package org.jpc.engine.embedded.indexing;

import org.jpc.term.Term;

public interface IndexChangeListener {

	public void onIndexChange(UpdatableIndexFunction<Term, Object> updatableIndexFunction);
	
}
