package org.jpc;

import java.lang.reflect.Type;

import org.jpc.term.Term;
import org.jpc.term.jterm.JRefManager;

public class JpcProxy extends Jpc {

	private Jpc proxiedJpc;

	public JpcProxy(Jpc proxiedJpc) {
		this.proxiedJpc = proxiedJpc;
	}
	@Override
	public <T> T fromTerm(Term term, Type type) {
		return proxiedJpc.fromTerm(term, type);
	}

	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		return proxiedJpc.toTerm(object, termClass);
	}

	@Override
	public <T> T instantiate(Type targetType) {
		return proxiedJpc.instantiate(targetType);
	}

	@Override
	public Type getType(Term term) {
		return proxiedJpc.getType(term);
	}

	@Override
	public JRefManager getRefManager() {
		return proxiedJpc.getRefManager();
	}
	
	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return proxiedJpc.handleError(errorTerm, goal);
	}


}
