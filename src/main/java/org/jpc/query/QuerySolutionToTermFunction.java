package org.jpc.query;

import java.util.Map;

import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

import com.google.common.base.Function;

public class QuerySolutionToTermFunction implements Function<Map<String, Term>, Term> {

	private Term term;
	
	public QuerySolutionToTermFunction(TermConvertable<Term> termConvertable) {
		term = termConvertable.asTerm();
	}
	
	@Override
	public Term apply(Map<String, Term> solution) {
		return term.replaceVariables(solution);
	}

}
