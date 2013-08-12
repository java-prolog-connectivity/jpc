package org.jpc;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

public class JpcProxy implements Jpc {

	private Jpc proxiedJpc;

	public JpcProxy(Jpc proxiedJpc) {
		this.proxiedJpc = proxiedJpc;
	}

	@Override
	public <T> T fromTerm(Term term) {
		return proxiedJpc.fromTerm(term);
	}

	@Override
	public <T> T fromTerm(Term term, Type type) {
		return proxiedJpc.fromTerm(term, type);
	}

	@Override
	public Term toTerm(Object object) {
		return proxiedJpc.toTerm(object);
	}

	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		return proxiedJpc.toTerm(object, termClass);
	}

	@Override
	public Compound toTerm(Object name, List<? extends Object> args) {
		return proxiedJpc.toTerm(name, args);
	}

	@Override
	public ListTerm listTerm(Object... objects) {
		return proxiedJpc.listTerm(objects);
	}

	@Override
	public ListTerm listTerm(List<? extends Object> objects) {
		return proxiedJpc.listTerm(objects);
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
	public boolean handleError(Term errorTerm, Term goal) {
		return proxiedJpc.handleError(errorTerm, goal);
	}

}
