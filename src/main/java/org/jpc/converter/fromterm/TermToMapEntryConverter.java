package org.jpc.converter.fromterm;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.term.Term;

public class TermToMapEntryConverter<K,V> implements TermToObjectConverter<Map.Entry<K,V>> {

	private TermToObjectConverter<K> keyConverter;
	private TermToObjectConverter<V> valueConverter;
	private Class<? extends Entry<K,V>> entryClass;
	
	public TermToMapEntryConverter(TermToObjectConverter<K> keyConverter, TermToObjectConverter<V> valueConverter) {
		this(keyConverter, valueConverter, (Class<? extends Entry<K, V>>) AbstractMap.SimpleEntry.class);
	}
	
	public TermToMapEntryConverter(TermToObjectConverter<K> keyConverter, TermToObjectConverter<V> valueConverter, Class<? extends Entry<K,V>> entryClass) {
		this.keyConverter = keyConverter;
		this.valueConverter = valueConverter;
		this.entryClass = entryClass;
	}
	
	@Override
	public Entry<K, V> apply(Term input) {
		K key = keyConverter.apply(input.arg(1));
		V value = valueConverter.apply(input.arg(2));
		Constructor<Entry<K, V>> entryConstructor;
		try {
			entryConstructor = (Constructor<Entry<K, V>>) entryClass.getConstructor(Object.class, Object.class);
			return entryConstructor.newInstance(key, value);
		} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}
	
}
