package org.jpc.query;

import java.util.Map;

import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	public static final String EXCEPTION_VAR_NAME = "JPC_EXCEPTION_VAR";
	

	
	private final Function<Map<String,Term>, Map<String,Term>> exceptionAdapterFunction = new Function<Map<String,Term>, Map<String,Term>>() {
		@Override
		public Map<String,Term> apply(Map<String,Term> solution) {
			if(solution.containsKey(EXCEPTION_VAR_NAME)) {
				Term exceptionTerm = solution.get(EXCEPTION_VAR_NAME);
				if(!(exceptionTerm instanceof Variable))
					getJpcContext().handleError(getPrologEngine(), exceptionTerm, getGoal());
			}
			return solution;
		}
	};

	public ExceptionHandledQuery(Query query) {
		super(query);
		adapterFunction = exceptionAdapterFunction;
	}

	@Override
	public Term getGoal() {
		return query.getGoal();
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return query.getGoal();
	}
	
}
