package org.jpc.converter.fromterm.fromlistterm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.converter.fromterm.TermToMapEntryConverter;
import org.jpc.term.Term;

public class ListTermToMapConverter<T extends Map> extends ListTermToObjectConverter<T> {

	private Class<T> mapClass;
	
	public ListTermToMapConverter(TermToMapEntryConverter mapEntryConverter) {
		this(mapEntryConverter, (Class<T>) HashMap.class);
	}
	
	public ListTermToMapConverter(TermToMapEntryConverter mapEntryConverter, Class<T> mapClass) {
		super(mapEntryConverter);
		this.mapClass = mapClass;
	}
	
	@Override
	public T apply(Term listTerm) {
		T map = null;
		try {
			map = mapClass.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		List<Term> listMembers = listTerm.asList();
		for(Term term : listMembers) {
			Entry entry = (Entry) getMemberConverter().apply(term);
			map.put(entry.getKey(), entry.getValue());
		}
		return map;
	}

}
