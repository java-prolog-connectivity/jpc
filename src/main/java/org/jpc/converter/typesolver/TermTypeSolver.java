package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

import org.jpc.term.Term;

/**
 * Maps native Prolog terms to an "equivalent" Java type, given certain structural properties of a logic term.
 * @author sergioc
 *
 */
public interface TermTypeSolver {
	
	public abstract Type getType(Term term);
	
}
