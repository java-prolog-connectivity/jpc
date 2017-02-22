package org.jpc.query;

import java.util.function.Function;

import org.jpc.term.Term;

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
