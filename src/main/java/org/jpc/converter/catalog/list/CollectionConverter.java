package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class CollectionConverter<E> extends JpcConverter<Collection<E>, Term> {
	
	@Override
	public Collection<E> fromTerm(Term listTerm, Type type, Jpc context) {
		Collection<E> collection = null;
		List<Term> listMembers = null;
		try {
			collection = context.instantiate(type); //instantiate the collection type
			listMembers = listTerm.asList();
		} catch(Exception e) {
			throw new JpcConversionException();
		}
		Type memberType = TypeWrapper.wrap(type).as(Collection.class).getActualTypeArgumentsOrUpperBounds()[0];
		for(Term term : listMembers) {
			E convertedMember = context.fromTerm(term, memberType);
			collection.add(convertedMember);
		}
		return collection;
	}

}
