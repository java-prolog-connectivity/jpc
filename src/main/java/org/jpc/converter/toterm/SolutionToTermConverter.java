package org.jpc.converter.toterm;

import java.util.Map;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import com.google.common.base.Function;

public class SolutionToTermConverter implements ToTermConverter<Map<String, Term>> {

	private Term term;
	
	public SolutionToTermConverter(TermConvertable<Term> termConvertable) {
		term = termConvertable.asTerm();
	}
	
	@Override
	public Term apply(Map<String, Term> solution) {
		return term.replaceVariables(solution);
	}

}
