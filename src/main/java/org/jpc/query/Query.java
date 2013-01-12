package org.jpc.query;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.converter.toterm.SolutionToTermConverter;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

/**
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author sergioc
 *
 */
public abstract class Query extends Cursor<Map<String,Term>> implements TermConvertable<Term> {

	private Term goal;
	private PrologEngine logicEngine;
	
	
	public Query(PrologEngine logicEngine, TermConvertable termConvertable) {
		this.logicEngine = logicEngine;
		this.goal = termConvertable.asTerm();
	}

	public Term asTerm() {
		return goal;
	}

	protected PrologEngine getLogicEngine() {
		return logicEngine;
	}

	public synchronized Cursor<Term> select(String selector) {
		return select(logicEngine.asTerm(selector));
	}
	
	/**
	 * Selects a cursor of terms. Each row in the cursor corresponds to a different binding of the term free variables according to a given solution
	 * @param selector a term with free variables
	 * @return
	 */
	public synchronized Cursor<Term> select(Term selector) {
		return adapt(new SolutionToTermConverter(selector));
	}
	
	public synchronized Multimap<String, Term> allSolutionsMultimap() {
		Multimap<String, Term> allSolutionsMultimap = ArrayListMultimap.create();
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

	@Override
	public String toString() {
		return goal.toString();
	}
	
}
