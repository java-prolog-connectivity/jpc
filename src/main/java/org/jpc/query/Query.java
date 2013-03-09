package org.jpc.query;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

/**
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author sergioc
 *
 */

public abstract class Query extends Cursor<Map<String,Term>> {

	protected abstract Term asTerm(String termString);
	
	public abstract PrologEngine getPrologEngine();
	
	public abstract Term goal();
	
	/**
	 * The default selected term when select* methods are called without specifying a select term
	 * By default, a PrologEngineQuery uses its goal, and QueryAdapter queries, its adapted default selected term
	 * Other QueryAdapter classes may find useful to override this method.
	 * For example, ExceptionHandledQuery overrides this method so it is the first argument in the catch block of the original goal.
	 * Logtalk queries overrides this so the default selected term is the Logtalk method and parameters (excluding the name of the Logtalk object and the :: operator)
	 * @return
	 */
	protected abstract Term getDefaultSelectedTerm();
	
	public synchronized Cursor<Term> select(String selector) {
		return select(asTerm(selector));
	}
	
	public synchronized Cursor<Term> select() {
		return select(getDefaultSelectedTerm());
	}
	
	/**
	 * Selects a cursor of terms. Each row in the cursor corresponds to a different binding of the term free variables according to a given solution
	 * @param selector a term with free variables
	 * @return
	 */
	public synchronized Cursor<Term> select(Term selector) {
		return adapt(new QuerySolutionToTermFunction(selector));
	}
	
	public synchronized <O> Cursor<O> selectObject() {
		return selectObject(getDefaultSelectedTerm());
	}
	
	public synchronized <O> Cursor<O> selectObject(Type targetType) {
		return selectObject(getDefaultSelectedTerm(), targetType);
	}
	
	public synchronized <O> Cursor<O> selectObject(String selector) {
		return selectObject(asTerm(selector), Object.class);
	}
	
	public synchronized <O> Cursor<O> selectObject(String selector, Type targetType) {
		return selectObject(asTerm(selector), targetType);
	}
	
	public synchronized <O> Cursor<O> selectObject(Term selector) {
		return selectObject(selector, Object.class);
	}

	public synchronized <O> Cursor<O> selectObject(Term selector, Type targetType) {
		return select(selector).adapt(getTermToObjectFunction(targetType));
	}
	
	protected abstract TermToObjectFunction getTermToObjectFunction(Type targetType);
	
	public synchronized ListMultimap<String, Term> allSolutionsMultimap() {
		ListMultimap<String, Term> allSolutionsMultimap = ArrayListMultimap.create();
		List<Map<String, Term>> allSolutions = allSolutions();
		for(Map<String, Term> solution : allSolutions) {
			for(Entry<String, Term> entry : solution.entrySet()) {
				allSolutionsMultimap.put(entry.getKey(), entry.getValue());
			}
		}
		return allSolutionsMultimap;
	}


	/**
	 * Answers if there are still solutions to the query
	 * In case there are no more solutions, the query will be closed by this method
	 * @return  true if there are more solutions to the query
	 */
	@Override
	public abstract boolean hasNext();

	
	/**
	 * This method returns an instance of java.util.Map, which represents
	 * a set of bindings from the names of query variables to terms within the solution.
	 * <p>
	 * For example, if a Query has an occurrence of a Variable,
	 * say, named "X", one can obtain the Term bound to "X" in the solution
	 * by looking up "X" in the Map.
	 * <pre>
	 * Variable x = new Variable("X");
	 * Query q = // obtain Query reference (with x in the Term array)
	 * while (q.hasMoreSolutions()) {
	 *     Map<String, Term> solution = q.nextSolution();
	 *     // make t the Term bound to "X" in the solution
	 *     Term t = solution.get("X");
	 *     // ...
	 * }
	 */
	@Override
	public abstract Map<String, Term> next();
	
}
