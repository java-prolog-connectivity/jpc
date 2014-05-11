package org.jpc.term.compiler;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.AbstractVar;
import org.jpc.term.Var;

/**
 * A compilation context keeps track of the ids assigned to variables as a map of strings (the variable names) to integers (the ids).
 * It also keeps a bidirectional map of var to compiled vars.
 * @author sergioc
 *
 */
public class CompilationContext {
	
	private final Map<String, AbstractVar> varMap;
	private int varCount;
	
	public CompilationContext() {
		varMap = new HashMap<>();
	}

	
	/**
	 * Compiles a variable for the given clause id according to this compilation context.
	 * @param var a variable to compile.
	 * @param env a clause environment.
	 * @return the compiled variable.
	 */
	public CompiledVar compile(Var var, Environment env) {
		String varName = var.getName();
		CompiledVar compiledVar = (CompiledVar)varMap.get(varName);
		if(compiledVar == null) {
			if(!var.isAnonymous()) {
				compiledVar = new CompiledVar(env.getId(), varCount++);
				varMap.put(varName, compiledVar);
			} else {
				compiledVar = CompiledVar.anonymousVar(env.getId());
			}
		}
		return compiledVar;
	}

	public BindableVar compileForFrame(AbstractVar var) {
		String varName = var.getName();
		BindableVar bindableVar = (BindableVar)varMap.get(varName);
		if(bindableVar == null) {
			bindableVar = new BindableVar(var);
			if(!var.isAnonymous())
				varMap.put(varName, bindableVar);
		}
		return bindableVar;
	}

}

