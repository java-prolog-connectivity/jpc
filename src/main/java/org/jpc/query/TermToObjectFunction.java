package org.jpc.query;

import java.lang.reflect.Type;

import org.jpc.DefaultJpc;
import org.jpc.Jpc;
import org.jpc.term.Term;

import com.google.common.base.Function;

public class TermToObjectFunction<T> implements Function<Term,T> {

	private Jpc context;
	private Type targetType;
	
	public TermToObjectFunction() {
		this(new DefaultJpc(), Object.class);
	}
	
	public TermToObjectFunction(Jpc context) {
		this(context, Object.class);
	}
	
	public TermToObjectFunction(Type targetType) {
		this(new DefaultJpc(), targetType);
	}
	
	public TermToObjectFunction(Jpc context, Type targetType) {
		this.context = context;
		this.targetType = targetType;
	}
	
	public T apply(Term term) {
		return context.fromTerm(term, targetType);
	}
	
}
