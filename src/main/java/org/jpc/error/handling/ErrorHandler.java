package org.jpc.error.handling;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public interface ErrorHandler {
	
	public abstract boolean handle(PrologEngine prologEngine, Term errorTerm, Term goal, Jpc context);

}
