package org.jpc.util;

import static java.util.Arrays.asList;

import java.util.Arrays;
import java.util.List;

import org.jpc.engine.prolog.PrologConstants;
import org.jpc.term.Compound;
import org.jpc.term.Term;

import com.google.common.base.Preconditions;

public class TermJoiner {

	public static TermJoiner on(String sep) {
		return new TermJoiner(sep);
	}
	
	public static TermJoiner getDefault() {
		return new TermJoiner(PrologConstants.SEQUENCE_SEPARATOR);
	}
	
	private final String sep;
	
	private TermJoiner(String sep) {
		this.sep = sep;
	}
	
	public Term join(List<Term> terms) {
		Preconditions.checkState(!terms.isEmpty(), "A sequence cannot be generated if the list is empty");
		Term termSequence = terms.get(terms.size()-1);
		for(int i = terms.size()-2; i>=0; i--) {
			termSequence = new Compound(sep, asList(terms.get(i), termSequence));
		}
		return termSequence;
	}
	
	public Term join(Term ...terms) {
		return join(Arrays.asList(terms));
	}
	
}
