package org.jpc.term;

/**
 * Implementors can be adapted to terms
 * @author sergioc
 *
 */
public interface TermConvertable<T extends Term> {
	public T asTerm();
}
