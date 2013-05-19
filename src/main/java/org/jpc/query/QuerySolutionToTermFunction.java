package org.jpc.query;

import org.jpc.term.Term;

import com.google.common.base.Function;

public class QuerySolutionToTermFunction implements Function<QuerySolution, Term> {

	private Term term;
	
	public QuerySolutionToTermFunction(Term term) {
		this.term = term;
	}
	
	@Override
	public Term apply(QuerySolution solution) {
		return term.replaceVariables(solution);
	}

}
