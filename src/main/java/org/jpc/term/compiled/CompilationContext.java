package org.jpc.term.compiled;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Var;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

/**
 * A compilation context keeps track of the ids assigned to variables as a map of strings (the variable names) to integers (the ids).
 * It also keeps a bidirectional map of var to compiled vars.
 * @author sergioc
 *
 */
public class CompilationContext {
	
	private final BiMap<Var, CompiledVar> compilationMap;
	private final Map<String, Integer> idMap;
	private int varCount;
	
	public CompilationContext() {
		compilationMap = HashBiMap.create();
		idMap = new HashMap<>();
	}
	
	private int getVarId(Var var) {
		String varName = var.getName();
		Integer varId = idMap.get(varName);
		if(varId == null) {
			varId = varCount++;
			idMap.put(varName, varId);
		}
		return varId;
	}
	
	/**
	 * Compiles a variable for the given clause id according to this compilation context.
	 * @param var a variable to compile.
	 * @param clauseId a clause id.
	 * @return the compiled variable.
	 */
	public CompiledVar compile(Var var, int clauseId) {
		if(var.isAnonymous())
			return CompiledVar.anonymousVar(clauseId);
		else {
			CompiledVar compiledVar = new CompiledVar(clauseId, getVarId(var));
			compilationMap.put(var, compiledVar);
			return compiledVar;
		}
	}
	
	/**
	 * Compiles a variable for a query according to this compilation context.
	 * @param var a variable to compile.
	 * @return the compiled variable.
	 */
	public CompiledVar compileForQuery(Var var) {
		if(var.isAnonymous())
			return CompiledVar.anonymousVar(QueryVar.QUERY_CODE);
		else {
			CompiledVar compiledVar = new QueryVar(var.getName(), getVarId(var));
			compilationMap.put(var, compiledVar);
			return compiledVar;
		}
	}
	
	public BiMap<Var, CompiledVar> getCompilationMap() {
		return compilationMap;
	}
	
}

