package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.HashMap;
import java.util.Map;


public class CompiledVar extends AbstractVar {

	public static final int ANONYMOUS_VAR_CODE = -1;
	
	protected final int clauseId;
	protected final int varId;
	
	public CompiledVar(int clauseId) {
		this(clauseId, ANONYMOUS_VAR_CODE); //the anonymous var
	}
	
	public CompiledVar(int clauseId, int varId) {
		this.clauseId = clauseId;
		this.varId = varId;
	}

	@Override
	public boolean isAnonymous() {
		return varId == ANONYMOUS_VAR_CODE;
	}
	
	@Override
	public String getName() {
		if(isAnonymous())
			return ANONYMOUS_VAR_NAME;
		return "_" + clauseId + "_"+ varId;
	}

	@Override
	public Term compile(int clauseId, CompilationContext context) {
		return this;
	}

	@Override
	public Term compileForQuery() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term forEnvironment(int environmentId) {
		return new EnvironmentVariable(clauseId, varId, environmentId);
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + clauseId;
		result = prime * result + varId;
		return result;
	}

	@Override
	public boolean termEquals(Term term) {
		if (this == term)
			return true;
		if (term == null)
			return false;
		if (getClass() != term.getClass())
			return false;
		CompiledVar other = (CompiledVar) term;
		if (clauseId != other.clauseId)
			return false;
		if (varId != other.varId)
			return false;
		return true;
	}

	
	
	public static class CompilationContext {
		
		private final Map<String, Integer> idMap;
		private int varCount;
		
		public CompilationContext() {
			idMap = new HashMap<>();
		}
		
		public CompiledVar compile(Var var, int clauseId) {
			if(var.isAnonymous())
				return new CompiledVar(clauseId);
			
			String varName = var.getName();
			Integer varId = idMap.get(varName);
			if(varId == null) {
				varId = varCount++;
				idMap.put(varName, varId);
			}
			return new CompiledVar(clauseId, varId);
		}
		
	}
	
	
	
	//work in progress...
	public static class EnvironmentVariable extends CompiledVar {

		private final int environmentId;
		
		public EnvironmentVariable(int clauseId, int varId, int environmentId) {
			super(clauseId, varId);
			this.environmentId = environmentId;
		}
		
		@Override
		public Term compile(int clauseId, CompilationContext context) {
			throw new UnsupportedOperationException();
		}

		@Override
		public Term compileForQuery() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Term forEnvironment(int environmentId) {
			throw new UnsupportedOperationException();
		}
		
		@Override
		public boolean termEquals(Term term) {
			if (this == term)
				return true;
			if (term == null)
				return false;
			if (getClass() != term.getClass())
				return false;
			EnvironmentVariable other = (EnvironmentVariable) term;
			if (clauseId != other.clauseId)
				return false;
			if (varId != other.varId)
				return false;
			if (environmentId != other.environmentId)
				return false;
			return true;
		}
		
	}
	
}
