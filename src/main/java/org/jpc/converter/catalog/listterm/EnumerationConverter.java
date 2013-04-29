package org.jpc.converter.catalog.listterm;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class EnumerationConverter<E> extends JpcConverter<Enumeration<E>, Term> {

	@Override
	public <T extends Term> T toTerm(Enumeration<E> en, Class<T> termClass, Jpc context) {
		return new IterableConverter<E>().toTerm(Collections.list(en), termClass, context);
	}
	
	@Override
	public Enumeration<E> fromTerm(Term term, Type type, Jpc context) {
		Type elementType = null;
		try {
			elementType = TypeWrapper.wrap(type).as(Enumeration.class).getActualTypeArgumentsOrUpperBounds()[0]; //will throw an exception if the type is not compatible with Enumeration
		} catch(Exception e) {
			throw new JpcConversionException();
		}
		
		Type listType = new ParameterizedTypeImpl(new Type[]{elementType}, null, List.class);
		List<E> collection = context.fromTerm(term, listType);
		return Collections.enumeration(collection);
	}
	
}
