package org.jpc.term.jterm;

import java.util.HashMap;
import java.util.Map;

import org.jpc.term.Compound;

import com.google.common.base.Function;

/**
 * Instances of this class are maps of term-indexes to terms.
 * The index of a term is obtained by means of an index function provided in the constructor.
 * Such index is an arbitrary object uniquely identifying a term from other terms stored in the same map .
 * The purpose of indexes is to provide an efficient mechanism for manipulating terms stored in the internal hash table.
 * 
 * @author sergioc
 *
 */
public class RefMap {

	private final Map<Object, JTerm<?>> map; //stores a set of JTerm objects uniquely identified by a term index (the key of the map). The term index is derived from the term representation of the reference.
	private final Function<Compound, Object> indexFunction; //answers the index of a given compound term.
	
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
