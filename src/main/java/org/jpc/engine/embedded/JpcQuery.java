package org.jpc.engine.embedded;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

import org.jpc.Jpc;
import org.jpc.query.PrologQuery;
import org.jpc.query.Solution;
import org.jpc.term.AbstractVar;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.compiled.CompilationContext;
import org.jpc.term.compiled.CompiledVar;

import com.google.common.collect.BiMap;

public class JpcQuery extends PrologQuery {

	private final Term compiledQuery;
	private final CompilationContext queryCompilationContext;
	private Iterator<Clause> clausesIt;
	
	public JpcQuery(JpcEngine prologEngine, Term goal, boolean errorHandledQuery, Jpc context) {
		super(prologEngine, goal, errorHandledQuery, context);
		queryCompilationContext = new CompilationContext();
		//compiledQuery = getInstrumentedGoal().compileForQuery(queryCompilationContext); //the instrumented goal makes use of predicates that are currently not available in the embedded Jpc engine.
		compiledQuery = getGoal().compileForQuery(queryCompilationContext);
	}
	
	
	@Override
	public boolean isAbortable() {
		return false;
	}
	
	@Override
	protected void basicAbort() {
		throw new UnsupportedOperationException();
	}

	
	@Override
	protected void basicClose() {
		clausesIt = null;
	}

	@Override
	protected List<Solution> basicAllSolutions() {
		return iterativeAllSolutions();
	}
	
	@Override
	public Solution basicNext() {
		if(clausesIt == null) {
			clausesIt = ((JpcEngine)getPrologEngine()).getClauseDatabase().clausesIterator(compiledQuery);
		}
		if(clausesIt.hasNext()) {
			Clause clause = clausesIt.next();
			Map<AbstractVar, Term> allVars = compiledQuery.unifyVars(clause.getHead());
			Map<String, Term> solutionMap = filterSolution(allVars, queryCompilationContext);
			Solution querySolution = new Solution(solutionMap, getPrologEngine(), getJpcContext());
			return querySolution;
		} else {
			throw new NoSuchElementException();
		}
	}
	
	/**
	 * Filters a solution so it keeps only the unbound variables originally in the query.
	 * Also format the solution in the most convenient structure Map<String, Term>
	 * @param allVars
	 * @param queryCompilationContext
	 * @return
	 */
	private Map<String, Term> filterSolution(Map<AbstractVar, Term> allVars, CompilationContext queryCompilationContext) {
		Map<String, Term> solution = new HashMap<>();
		BiMap<Var, CompiledVar> compilationMap = queryCompilationContext.getCompilationMap();
		BiMap<CompiledVar, Var> inverseCompilationMap = compilationMap.inverse();
		for(Entry<AbstractVar, Term> varEntry : allVars.entrySet()) {
			AbstractVar compiledVar = varEntry.getKey();
			Var originalQueryVar = inverseCompilationMap.get(compiledVar);
			if(originalQueryVar != null) {
				solution.put(originalQueryVar.getName(), varEntry.getValue());
			}
		}
		return solution;
	}
}
