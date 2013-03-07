package org.jpc.query;

import java.util.Map;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Term;

import com.google.common.base.Function;

public class QuerySolutionToTermFunction implements Function<Map<String, Term>, Term> {

	private Term term;
	
	public QuerySolutionToTermFunction(Term term) {
		this.term = term;
	}
	
	@Override
	public Term apply(Map<String, Term> solution) {
		return term.replaceVariables(solution);
	}

}
