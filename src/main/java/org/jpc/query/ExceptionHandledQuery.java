package org.jpc.query;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	private final Function<Solution, Solution> exceptionAdapterFunction = new Function<Solution, Solution>() {
		@Override
		public Solution apply(Solution solution) {
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
