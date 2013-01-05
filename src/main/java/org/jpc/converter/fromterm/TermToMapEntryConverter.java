package org.jpc.converter.fromterm;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.term.Term;

public class TermToMapEntryConverter<K,V> implements TermToObjectConverter<Map.Entry<K,V>> {

	private TermToObjectConverter<K> keyConverter;
	private TermToObjectConverter<V> valueConverter;
	
	public TermToMapEntryConverter() {
		this((TermToObjectConverter<K>)new DefaultTermToObjectConverter(), (TermToObjectConverter<V>)new DefaultTermToObjectConverter());
	}
	
	public TermToMapEntryConverter(TermToObjectConverter<K> keyConverter, TermToObjectConverter<V> valueConverter) {
		this.keyConverter = keyConverter;
		this.valueConverter = valueConverter;
	}
	
	@Override
	public Entry<K, V> apply(Term input) {
		K key = keyConverter.apply(input.arg(1));
		V value = valueConverter.apply(input.arg(2));
		return new AbstractMap.SimpleEntry(key, value);
	}
	
}
