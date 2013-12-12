package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class EnumerationConverter<T extends Term> implements BidirectionalTermConverter<Enumeration, T> {

	@Override
	public T toTerm(Enumeration en, Class<T> termClass, Jpc context) {
		return (T) new IterableConverter().toTerm(Collections.list(en), termClass, context);
	}
	
	@Override
	public Enumeration fromTerm(T term, Type type, Jpc context) {
		Type elementType = null;
		try {
			elementType = TypeWrapper.wrap(type).as(Enumeration.class).getActualTypeArgumentsOrUpperBounds()[0]; //will throw an exception if the type is not compatible with Enumeration
		} catch(Exception e) {
			throw new ConversionException();
		}
		
		Type listType = new ParameterizedTypeImpl(new Type[]{elementType}, null, List.class);
		List collection = context.fromTerm(term, listType);
		return Collections.enumeration(collection);
	}
	
}
