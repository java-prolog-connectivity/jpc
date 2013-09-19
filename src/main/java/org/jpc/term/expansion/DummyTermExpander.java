package org.jpc.term.expansion;

import org.jpc.term.Term;

import com.google.common.base.Optional;

/**
 * This class should be deleted in Java 8 when default methods are available
 * @author sergioc
 *
 */
public class DummyTermExpander implements TermExpander {

	public Optional<Term> expand(Term term) {
		return Optional.absent();
	}
	
}
