package org.jpc.term.compiler;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.AbstractVar;
import org.jpc.term.Var;

public class Environment {

	private static int ID_COUNTER = -1;
	
	private final int id;
	
	private final Map<String, AbstractVar> varMap;
	private int varCount;
	
	
	public Environment() {
		this(ID_COUNTER--); //in the current implementation default environment ids are negative numbers, positive ids are reserved for clause environments.
	}
	
	public Environment(int id) {
		this.id = id;
		varMap = new HashMap<>();
	}
	
	public int getId() {
		return id;
	}

	/**
	 * Compiles a variable for the given clause id according to this compilation context.
	 * @param var a variable to compile.
	 * @param env a clause environment.
	 * @return the compiled variable.
	 */
	public CompiledVar compile(Var var) {
		String varName = var.getName();
		CompiledVar compiledVar = (CompiledVar)varMap.get(varName);
		if(compiledVar == null) {
			if(!var.isAnonymous()) {
				compiledVar = new CompiledVar(getId(), varCount++);
				varMap.put(varName, compiledVar);
			} else {
				compiledVar = CompiledVar.anonymousVar(getId());
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

	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ (id >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Environment other = (Environment) obj;
		if (id != other.id)
			return false;
		return true;
	}
	
}
