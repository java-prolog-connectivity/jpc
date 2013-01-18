package org.jpc.converter.fromterm.fromlistterm;

import java.util.ArrayList;
import java.util.List;

import org.jpc.converter.fromterm.DefaultFromTermConverter;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;

public class ListTermToListConverter<T> extends ListTermToObjectConverter<List<T>> {
	
	public ListTermToListConverter() {
		this((FromTermConverter<T>) new DefaultFromTermConverter());
	}
	
	public ListTermToListConverter(FromTermConverter<T> memberConverter) {
		super(memberConverter);
	}

	@Override
	public List<T> apply(Term listTerm) {
		List<T> collection = new ArrayList<>();
		List<Term> listMembers = listTerm.asList();
		for(Term term : listMembers) {
			T convertedMember = (T) getMemberConverter().apply(term);
			collection.add(convertedMember);
		}
		return collection;
	}

}
