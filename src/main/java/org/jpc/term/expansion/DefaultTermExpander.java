package org.jpc.term.expansion;

import org.jpc.term.Term;

import com.google.common.base.Function;

/**
 * This class should be deleted in Java 8 when default methods are available
 * @author sergioc
 *
 */
public class DefaultTermExpander implements Function<Term, Term> {

	public Term apply(Term term) {
		return null;
	}
	
}
