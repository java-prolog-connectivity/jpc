package org.jpc.engine.prolog;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

/**
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author sergioc
 *
 */
public abstract class Query implements AutoCloseable, Iterator<Map<String, Term>> {

	private Term goal;
	
	public Query(TermConvertable termConvertable) {
		this.goal = termConvertable.asTerm();
	}

	public Term goal() {
		return goal;
	}

	/**
	 * isOpen() returns true iff the query is open.
	 * @return	true if the query is open, otherwise false.
	 */
	public abstract  boolean isOpen();

	public abstract void abort();
	//public abstract void open();
	public abstract void close();
	
	public synchronized boolean hasSolution() {
		return oneSolution() != null;
	}
	
	public synchronized long numberOfSolutions() {
		return allSolutions().size();
	}
	
	public synchronized Map<String, Term> oneSolution() {
		try {
			return nSolutions(1).get(0);
		} catch(IndexOutOfBoundsException e) {
			return null;
		}
	}
	
	/**
	 * 
	 * @param n the number of solutions the method should return
	 * @return is the number of solutions
	 */
	public synchronized List<Map<String, Term>> nSolutions(long n) {
		return solutionsRange(0, n);
	}


	/**
	 * 
	 * @param from the (0-based) index (inclusive) of the first solution
	 * @param to the (0-based) index (exclusive) of the last solution
	 * @return an array with the solutions according to the indexes sent as parameters
	 */
	public synchronized List<Map<String, Term>> solutionsRange(long from, long to) {
		checkArgument(from >= 0);
		checkArgument(to > from);
		List<Map<String, Term>> solutions = new ArrayList<>();
		if (isOpen()) {
			throw new JpcException("Query is already open");
		} else {
			try (Query queryToClose = this){
				long count = 0;
				while(count<to) {
					Map<String, Term> solution = null;
					if(hasNext()) {
						solution = next();
						if(count >= from)
							solutions.add(solution);
					}
					else
						throw new IndexOutOfBoundsException("The query " + this + "has only " + count + " solutions");
					count++;
				}
			}
			return solutions;
		}
	}
	
	/**
	 * calls the Query's goal to exhaustion
	 * The query should not be open when this method is called
	 * and returns an array of zero or more Maps of zero or more variablename-to-term bindings (each Map represents a solution, in the order in which they were found).
	 * @return an array of zero or more Hashtables of zero or more variablename-to-term bindings (each Map represents a solution, in the order in which they were found)
	 */
	public synchronized List<Map<String, Term>> allSolutions() {
		if (isOpen()) {
			throw new JpcException("Query is already open");
		} else {
			List<Map<String, Term>> allSolutions = new ArrayList<>();
			while (hasNext()) { 
				allSolutions.add(next());
			}
			return allSolutions;
		}
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
	 *     Hashtable solution = q.nextSolution();
	 *     // make t the Term bound to "X" in the solution
	 *     Term t = (Term) solution.get("X");
	 *     // ...
	 * }
	 */
	@Override
	public abstract Map<String, Term> next();
	
	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}



	@Override
	public String toString() {
		return goal.toString();
	}
	
	
}
