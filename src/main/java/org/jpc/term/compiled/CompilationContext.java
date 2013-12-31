package org.jpc.term.compiled;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Var;

public class CompilationContext {
	
	private final Map<String, Integer> idMap;
	private int varCount;
	
	public CompilationContext() {
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
		else
			return new CompiledVar(clauseId, getVarId(var));
	}
	
	public CompiledVar compileForQuery(Var var) {
		if(var.isAnonymous())
			return CompiledVar.anonymousVar(QueryVar.QUERY_CODE);
		else
			return new QueryVar(var.getName(), getVarId(var));
	}
	
}