package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class CollectionConverter<U extends Term,T extends Collection> implements FromTermConverter<U,T> {
	
	@Override
	public T fromTerm(Term listTerm, Type type, Jpc context) {
		T collection = null;
		List<Term> listMembers = null;
		try {
			collection = context.instantiate(type); //instantiate the collection type
			listMembers = listTerm.asList();
		} catch(Exception e) {
			throw new ConversionException();
		}
		
		TypeWrapper listTypeWrapper = TypeWrapper.wrap(type);
		Type componentType;
		if(listTypeWrapper.hasActualTypeArguments()) {
			componentType = listTypeWrapper.getActualTypeArguments()[0];
		} else {
			componentType = Object.class;
		}
		
		for(Term term : listMembers) {
			Object convertedMember = context.fromTerm(term, componentType);
			collection.add(convertedMember);
		}
		return collection;
	}

}
