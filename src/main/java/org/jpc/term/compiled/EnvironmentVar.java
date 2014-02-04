package org.jpc.term.compiled;

import org.jpc.term.Term;


public class EnvironmentVar extends CompiledVar {

	private final int environmentId;
	
	EnvironmentVar(int clauseId, int varId, int environmentId) {
		super(clauseId, varId);
		this.environmentId = environmentId;
	}
	
	
	@Override
	public boolean termEquals(Term term) {
		if (this == term)
			return true;
		if (term == null)
			return false;
		if (getClass() != term.getClass())
			return false;
		EnvironmentVar other = (EnvironmentVar) term;
		if (environmentId != other.environmentId)
			return false;
//		if (clauseId != other.clauseId) //if the environments are the same the clause id should also be the same
//			return false;
		if (varId != other.varId)
			return false;
		
		return true;
	}
	
	@Override
	public Term compile(int clauseId, CompilationContext context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term compileForQuery(CompilationContext context) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term forFrame(int frameId) {
		throw new UnsupportedOperationException();
	}

	
}