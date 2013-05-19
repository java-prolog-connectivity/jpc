package org.jpc.query;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;

public class QuerySolution implements Map<String,Term> {

	private PrologEngine prologEngine;
	private Map<String, Term> solution;
	private Jpc context;
	
	public QuerySolution(Map<String, Term> solution, PrologEngine prologEngine, Jpc context) {
		this.solution = solution;
		this.prologEngine = prologEngine;
		this.context = context;
	}
	
	@Override
	public int size() {
		return solution.size();
	}

	@Override
	public boolean isEmpty() {
		return solution.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return solution.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return solution.containsValue(value);
	}

	@Override
	public Term get(Object key) {
		return solution.get(key);
	}

	@Override
	public Term put(String key, Term value) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Term remove(Object key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void putAll(Map<? extends String, ? extends Term> m) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException();
	}

	@Override
	public Set<String> keySet() {
		return solution.keySet();
	}

	@Override
	public Collection<Term> values() {
		return solution.values();
	}

	@Override
	public Set<java.util.Map.Entry<String, Term>> entrySet() {
		return solution.entrySet();
	}
	
	public Byte getByte(String key) {
		return context.fromTerm(get(key), Byte.class);
	}
	
	public Short getShort(String key) {
		return context.fromTerm(get(key), Short.class);
	}
	
	public Integer getInt(String key) {
		return context.fromTerm(get(key), Integer.class);
	}
	
	public Long getLong(String key) {
		return context.fromTerm(get(key), Long.class);
	}
	
	public Float getFloat(String key) {
		return context.fromTerm(get(key), Float.class);
	}
	
	public Double getDouble(String key) {
		return context.fromTerm(get(key), Double.class);
	}
	
	public Boolean getBoolean(String key) {
		return context.fromTerm(get(key), Boolean.class);
	}
	
	public Character getChar(String key) {
		return context.fromTerm(get(key), Character.class);
	}
	
	public String getString(String key) {
		return context.fromTerm(get(key), String.class);
	}
	
	
	public <O> O asObject(String selector) {
		return asObject(asTerm(selector), Object.class);
	}
	
	public <O> O asObject(String selector, Type targetType) {
		return asObject(asTerm(selector), targetType);
	}
	
	public <O> O asObject(Term selector) {
		return asObject(selector, Object.class);
	}

	public <O> O asObject(Term selector, Type targetType) {
		return (O) new TermToObjectFunction(context, targetType).apply(selector);
	}

	private Term asTerm(String termString) {
		return prologEngine.asTerm(termString);
	}
	
}
