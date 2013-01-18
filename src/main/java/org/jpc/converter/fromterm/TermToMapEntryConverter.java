package org.jpc.converter.fromterm;

import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.term.Term;

public class TermToMapEntryConverter<K,V> implements FromTermConverter<Map.Entry<K,V>> {

	private FromTermConverter<K> keyConverter;
	private FromTermConverter<V> valueConverter;
	
	public TermToMapEntryConverter() {
		this((FromTermConverter<K>)new DefaultFromTermConverter(), (FromTermConverter<V>)new DefaultFromTermConverter());
	}
	
	public TermToMapEntryConverter(FromTermConverter<K> keyConverter, FromTermConverter<V> valueConverter) {
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
