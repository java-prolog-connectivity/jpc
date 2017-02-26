package org.jpc.mapping.converter;

import org.jpc.term.Term;

/**
 * Implementors can be adapted to terms
 * @author sergioc
 *
 */
public interface TermConvertable<T extends Term> {
	T asTerm();
}
