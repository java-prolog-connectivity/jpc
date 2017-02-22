package org.jpc.query;

import java.lang.reflect.Type;
import java.util.function.Function;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Term;

public class TermToObjectFunction<T> implements Function<Term,T> {

	private Jpc context;
	private Type targetType;
	
	public TermToObjectFunction() {
		this(JpcBuilder.create().build(), Object.class);
	}
	
	public TermToObjectFunction(Jpc context) {
		this(context, Object.class);
	}
	
	public TermToObjectFunction(Type targetType) {
		this(JpcBuilder.create().build(), targetType);
	}
	
	public TermToObjectFunction(Jpc context, Type targetType) {
		this.context = context;
		this.targetType = targetType;
	}
	
	public T apply(Term term) {
		return context.fromTerm(term, targetType);
	}
	
}
