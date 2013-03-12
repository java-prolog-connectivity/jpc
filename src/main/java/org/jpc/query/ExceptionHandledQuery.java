package org.jpc.query;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.CATCH;

import java.util.Map;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

import com.google.common.base.Function;

public class ExceptionHandledQuery extends QueryAdapter {

	private static final String EXCEPTION_VAR_NAME = "JPC_EXCEPTION_VAR";
	
	public static Term exceptionHandledQueryTerm(Term term) {
		return new Compound(CATCH, asList(term, new Variable(EXCEPTION_VAR_NAME), Atom.TRUE_TERM));
	}

	public static ExceptionHandledQuery create(PrologEngine prologEngine, Term term, Jpc context) {
		Query basicQuery = prologEngine.basicQuery(exceptionHandledQueryTerm(term), context);
		return new ExceptionHandledQuery(basicQuery, context);
	}
	

	
	private final Function<Map<String,Term>, Map<String,Term>> exceptionAdapterFunction = new Function<Map<String,Term>, Map<String,Term>>() {
		@Override
		public Map<String,Term> apply(Map<String,Term> solution) {
			if(solution.containsKey(EXCEPTION_VAR_NAME)) {
				Term exceptionTerm = solution.get(EXCEPTION_VAR_NAME);
				if(!(exceptionTerm instanceof Variable))
					context.handleError(getPrologEngine(), exceptionTerm, goal());
			}
			return solution;
		}
	};
	
	private final Jpc context;
	
	public ExceptionHandledQuery(Query query, Jpc context) {
		super(query);
		adapterFunction = exceptionAdapterFunction;
		this.context = context;
	}

	@Override
	public Term goal() {
		return ((Compound)query.goal()).arg(1);
	}
	
	@Override
	protected Term getDefaultSelectedTerm() {
		return ((Compound)query.goal()).arg(1);
	}
	
}
