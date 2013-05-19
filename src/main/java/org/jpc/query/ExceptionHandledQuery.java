package org.jpc.query;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	private final Function<QuerySolution, QuerySolution> exceptionAdapterFunction = new Function<QuerySolution, QuerySolution>() {
		@Override
		public QuerySolution apply(QuerySolution solution) {
			if(solution.isError())
				getJpcContext().handleError(solution.getErrorTerm(), getGoal());
			return solution;
		}
	};

	public ExceptionHandledQuery(Query query) {
		super(query);
		adapterFunction = exceptionAdapterFunction;
	}
	
}
