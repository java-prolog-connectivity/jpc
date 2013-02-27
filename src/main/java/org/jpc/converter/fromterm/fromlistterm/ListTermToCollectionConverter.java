package org.jpc.converter.fromterm.fromlistterm;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;

public class ListTermToCollectionConverter<T> extends FromTermConverter<Collection<T>> {
	
	@Override
	public Collection<T> convert(Term listTerm, Type type, Jpc context) {
		Collection<T> collection = null;
		List<Term> listMembers = null;
		try {
			collection = context.instantiate(type); //instantiate the collection type
			listMembers = listTerm.asList();
		} catch(Exception e) {
			throw new JpcConversionException();
		}
		Type memberType = TypeWrapper.wrap(type).as(Collection.class).getActualTypeArgumentsOrUpperBounds()[0];
		for(Term term : listMembers) {
			T convertedMember = context.fromTerm(term, memberType);
			collection.add(convertedMember);
		}
		return collection;
	}

}
