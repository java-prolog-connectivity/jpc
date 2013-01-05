package org.jpc.converter.fromterm.fromlistterm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.converter.fromterm.TermToMapEntryConverter;
import org.jpc.term.Term;

public class ListTermToMapConverter extends ListTermToObjectConverter<Map> {
	
	public ListTermToMapConverter() {
		this(new TermToMapEntryConverter());
	}
	
	public ListTermToMapConverter(TermToMapEntryConverter mapEntryConverter) {
		super(mapEntryConverter);
	}
	
	@Override
	public Map apply(Term listTerm) {
		Map map = new HashMap<>();
		List<Term> listMembers = listTerm.asList();
		for(Term term : listMembers) {
			Entry entry = (Entry) getMemberConverter().apply(term);
			map.put(entry.getKey(), entry.getValue());
		}
		return map;
	}

}
