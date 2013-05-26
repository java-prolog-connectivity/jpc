package org.jpc.query;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.jpc.Jpc;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class QuerySolution implements Map<String,Term> {

	public static final String EXCEPTION_VAR_NAME = "JPC_EXCEPTION_VAR";
	
	private PrologEngine prologEngine;
	private Map<String, Term> allVariablesSolution;
	private Map<String, Term> nonAnonymousVariablesSolution;
	private Jpc context;
	private Term errorTerm;
	
	public QuerySolution(Map<String, Term> rawSolution, PrologEngine prologEngine, Jpc context) {
		this.allVariablesSolution = new HashMap<>(rawSolution);
		this.prologEngine = prologEngine;
		this.context = context;
		configure(); //The original solution may be modified. 
		nonAnonymousVariablesSolution = nonAnonymousVariablesSolutions(allVariablesSolution);
	}
	
	private static Map<String, Term> nonAnonymousVariablesSolutions(Map<String, Term> allVariablesSolution) {
		Map<String, Term> nonAnonymousVariablesSolution = new HashMap<>();
		for(String varName : allVariablesSolution.keySet()) {
			if(!Variable.isAnonymousVariableName(varName)) {
				nonAnonymousVariablesSolution.put(varName, allVariablesSolution.get(varName));
			}
		}
		return nonAnonymousVariablesSolution;
	}
	
	/**
	 * Tracks and processes special variables in the solution. The original solution may be modified.
	 */
	private void configure() {
		if(allVariablesSolution.containsKey(EXCEPTION_VAR_NAME)) {
			Term errorTerm = allVariablesSolution.get(EXCEPTION_VAR_NAME);
			if(!(errorTerm instanceof Variable)) {
				this.errorTerm = errorTerm;
			}
			allVariablesSolution.remove(EXCEPTION_VAR_NAME);
		}
	}
	
	public boolean isError() {
		return errorTerm != null;
	}
	
	public Term getErrorTerm() {
		return errorTerm;
	}
	
	@Override
	public int size() {
		return size(false);
	}
	
	public int size(boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.size();
		else
			return nonAnonymousVariablesSolution.size();
	}

	@Override
	public boolean isEmpty() {
		return isEmpty(false);
	}

	public boolean isEmpty(boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.isEmpty();
		else
			return nonAnonymousVariablesSolution.isEmpty();
	}
	
	@Override
	public boolean containsKey(Object key) {
		return containsKey(key, false);
	}

	public boolean containsKey(Object key, boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.containsKey(key);
		else
			return nonAnonymousVariablesSolution.containsKey(key);
	}
	
	@Override
	public boolean containsValue(Object value) {
		return containsValue(value, false);
	}

	public boolean containsValue(Object value, boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.containsValue(value);
		else
			return nonAnonymousVariablesSolution.containsValue(value);
	}
	
	@Override
	public Term get(Object key) {
		return get(key, false);
	}

	public Term get(Object key, boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.get(key);
		else
			return nonAnonymousVariablesSolution.get(key);
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
		return keySet(false);
	}

	public Set<String> keySet(boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.keySet();
		else
			return nonAnonymousVariablesSolution.keySet();
	}
	
	@Override
	public Collection<Term> values() {
		return values(false);
	}

	public Collection<Term> values(boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.values();
		else
			return nonAnonymousVariablesSolution.values();
	}
	
	@Override
	public Set<java.util.Map.Entry<String, Term>> entrySet() {
		return entrySet(false);
	}
	
	public Set<java.util.Map.Entry<String, Term>> entrySet(boolean considerAllVariables) {
		if(considerAllVariables)
			return allVariablesSolution.entrySet();
		else
			return nonAnonymousVariablesSolution.entrySet();
	}
	
	
	public Byte getByte(String key) {
		return getObject(key, Byte.class);
	}
	
	public Short getShort(String key) {
		return getObject(key, Short.class);
	}
	
	public Integer getInt(String key) {
		return getObject(key, Integer.class);
	}
	
	public Long getLong(String key) {
		return getObject(key, Long.class);
	}
	
	public Float getFloat(String key) {
		return getObject(key, Float.class);
	}
	
	public Double getDouble(String key) {
		return getObject(key, Double.class);
	}
	
	public Boolean getBoolean(String key) {
		return getObject(key, Boolean.class);
	}
	
	public Character getChar(String key) {
		return getObject(key, Character.class);
	}
	
	public String getString(String key) {
		return getObject(key, String.class);
	}
	
	public <O> O getObject(String key) {
		return context.fromTerm(get(key, true));
	}
	
	public <O> O getObject(String key, Type type) {
		return context.fromTerm(get(key, true), type);
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
		Term boundTerm = selector.replaceVariables(allVariablesSolution);
		return (O) new TermToObjectFunction(context, targetType).apply(boundTerm);
	}

	private Term asTerm(String termString) {
		return prologEngine.asTerm(termString, context);
	}

}
