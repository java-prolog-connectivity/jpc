package org.jpc.term.compiled;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Var;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

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
	
	public CompiledVar compile(Var var, int clauseId) {
		if(var.isAnonymous())
			return CompiledVar.anonymousVar(clauseId);
		else {
			CompiledVar compiledVar = new CompiledVar(clauseId, getVarId(var));
			compilationMap.put(var, compiledVar);
			return compiledVar;
		}
	}
	
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

