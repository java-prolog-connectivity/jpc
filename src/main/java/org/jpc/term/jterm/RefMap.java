package org.jpc.term.jterm;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Compound;

import com.google.common.base.Function;

public class RefMap {

	private Map<Object, JTerm<?>> map;
	private Function<Compound, Object> indexFunction;
	
	public RefMap() {
		this(new Function<Compound, Object>() {
			@Override
			public Object apply(Compound term) { //identity function by default
				return term;
			}
		});
	}
	
	public RefMap(Function<Compound, Object> indexFunction) {
		this.map = new HashMap<>();
		this.indexFunction = indexFunction;
	}
	
	public boolean containsKey(Compound compound) {
		Object key = indexFunction.apply(compound);
		return map.containsKey(key);
	}

	public JTerm<?> remove(Compound compound) {
		Object key = indexFunction.apply(compound);
		return map.remove(key);
	}
	
	public JTerm<?> put(Compound compound, JTerm<?> jTerm) {
		Object key = indexFunction.apply(compound);
		return map.put(key, jTerm);
	}
	
	public JTerm<?> get(Compound compound) {
		Object key = indexFunction.apply(compound);
		return map.get(key);
	}

}
