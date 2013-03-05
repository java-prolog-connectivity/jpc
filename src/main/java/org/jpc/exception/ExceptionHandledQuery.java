package org.jpc.exception;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.CATCH;

import java.util.Map;

import org.jpc.query.Query;
import org.jpc.query.QueryAdapter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	private final ExceptionHandlerManager exceptionHandler;
	
	private static final String EXCEPTION_VAR_NAME = "JPC_EXCEPTION_VAR";
	private Term defaultCatch(Term term) {
		return new Compound(CATCH, asList(term, new Variable(EXCEPTION_VAR_NAME), Atom.TRUE_TERM));
	}
	
	private final Function<Map<String,Term>, Map<String,Term>> exceptionAdapterFunction = new Function<Map<String,Term>, Map<String,Term>>() {
		@Override
		public Map<String,Term> apply(Map<String,Term> solution) {
			if(solution.containsKey(EXCEPTION_VAR_NAME)) {
				Term exceptionTerm = solution.get(EXCEPTION_VAR_NAME);
				exceptionHandler.handle(getPrologEngine(), exceptionTerm, asTerm());
			}
			return solution;
		}
	};
	
	public ExceptionHandledQuery(Query query, ExceptionHandlerManager exceptionHandler) {
		super(query);
		adapterFunction = exceptionAdapterFunction;
		this.exceptionHandler = exceptionHandler;
	}

}
