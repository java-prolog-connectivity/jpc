package org.jpc.converter.fromterm.fromlistterm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.jpc.converter.fromterm.TermToObjectConverter;
import org.jpc.term.Term;

public class ListTermToCollectionConverter<T extends Collection> extends ListTermToObjectConverter<T> {

	private Class<T> collectionClass;
	
	public ListTermToCollectionConverter(TermToObjectConverter memberConverter) {
		this(memberConverter, (Class<T>) ArrayList.class);
	}
	
	public ListTermToCollectionConverter(TermToObjectConverter memberConverter, Class<T> collectionClass) {
		super(memberConverter);
		this.collectionClass = collectionClass;
	}

	@Override
	public T apply(Term listTerm) {
		T collection = null;
		try {
			collection = collectionClass.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		List<Term> listMembers = listTerm.asList();
		for(Term term : listMembers) {
			Object convertedMember = getMemberConverter().apply(term);
			collection.add(convertedMember);
		}
		return collection;
	}

}
