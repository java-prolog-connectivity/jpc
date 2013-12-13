package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class EnumerationConverter<T extends Term> implements ToTermConverter<Enumeration, T>, FromTermConverter<T, Enumeration> {

	@Override
	public T toTerm(Enumeration en, Class<T> termClass, Jpc context) {
		return (T) new IterableConverter().toTerm(Collections.list(en), termClass, context);
	}
	
	@Override
	public Enumeration fromTerm(T listTerm, Type type, Jpc context) {
		if(!listTerm.isList())
			throw new ConversionException();
		Type elementType = null;
		try {
			elementType = TypeWrapper.wrap(type).as(Enumeration.class).getActualTypeArgumentsOrUpperBounds()[0]; //will throw an exception if the type is not compatible with Enumeration
		} catch(Exception e) {
			throw new ConversionException();
		}
		
		Type listType = new ParameterizedTypeImpl(new Type[]{elementType}, null, List.class);
		List collection = context.fromTerm(listTerm, listType);
		return Collections.enumeration(collection);
	}
	
}
