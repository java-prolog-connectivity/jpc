package org.jpc.error.handling;

import org.jpc.Jpc;
import org.jpc.term.Term;

public interface ErrorHandler {
	
	boolean handle(Term errorTerm, Term goal, Jpc context);

}
