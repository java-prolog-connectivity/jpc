package org.jpc.query;

import org.jpc.term.Term;
import org.jpc.term.Variable;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	public static final String EXCEPTION_VAR_NAME = "JPC_EXCEPTION_VAR";
	
	private final Function<QuerySolution, QuerySolution> exceptionAdapterFunction = new Function<QuerySolution, QuerySolution>() {
		@Override
		public QuerySolution apply(QuerySolution solution) {
			if(solution.containsKey(EXCEPTION_VAR_NAME)) {
				Term exceptionTerm = solution.get(EXCEPTION_VAR_NAME);
				if(!(exceptionTerm instanceof Variable))
					getJpcContext().handleError(exceptionTerm, getGoal());
			}
			return solution;
		}
	};

	public ExceptionHandledQuery(Query query) {
		super(query);
		adapterFunction = exceptionAdapterFunction;
	}
	
}
