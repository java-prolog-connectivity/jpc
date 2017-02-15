package org.jpc.term.compiler;

import static org.jpc.engine.prolog.PrologConstants.UNDERSCORE_VAR_NAME;

import org.jpc.term.AbstractVar;
import org.jpc.term.Term;


public final class CompiledVar extends AbstractVar {

	public static final int ANONYMOUS_VAR_CODE = -1;
	
	protected final int clauseId;
	protected final int varId;
	
	/**
	 * Implementation note: At the moment an anonymous variable keeps the clause id of the clause defining it for traceability reasons (e.g., to facilitate debugging).
	 * @param clauseId the clause id where this anonymous variable was declared.
	 * @return a compiled anonymous variable.
	 */
	public static CompiledVar anonymousVar(int clauseId) {
		return new CompiledVar(clauseId, ANONYMOUS_VAR_CODE); //the anonymous var
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
			return UNDERSCORE_VAR_NAME;
		return "_" + clauseId + "_"+ varId;
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
	
	
	@Override
	public void unify(Term term) {
		throw new UncompiledTermException(this);
	}
	
	@Override
	public Term preCompile(Environment env) {
		return this;
	}

	@Override
	public Term prepareForQuery(Environment env) {
		throw new UncompiledTermException(this);
	}

	@Override
	public Term prepareForFrame(Environment env) {
		return env.compileForFrame(this);
	}
	
}
