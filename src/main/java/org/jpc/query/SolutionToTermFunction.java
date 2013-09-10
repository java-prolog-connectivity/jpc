package org.jpc.query;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class SolutionToTermFunction implements Function<Solution, Term> {

	private Term term;
	
	public SolutionToTermFunction(Term term) {
		this.term = term;
	}
	
	@Override
	public Term apply(Solution solution) {
		return term.replaceVariables(solution);
	}

}
