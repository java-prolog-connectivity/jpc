package org.jpc.converter.fromterm.fromlistterm;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;

public class ListTermToEnumerationConverter<T> extends FromTermConverter<Enumeration<T>> {

	@Override
	public Enumeration<T> convert(Term term, Type type, Jpc context) {
		Type elementType = null;
		try {
			elementType = TypeWrapper.wrap(type).as(Enumeration.class).getActualTypeArgumentsOrUpperBounds()[0]; //will throw an exception if the type is not compatible with Enumeration
		} catch(Exception e) {
			throw new JpcConversionException();
		}
		
		Type listType = new ParameterizedTypeImpl(new Type[]{elementType}, null, List.class);
		List<T> collection = context.fromTerm(term, listType);
		return Collections.enumeration(collection);
	}

}
