package org.jpc.mapping.converter.catalog.list;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;


public class EnumerationConverter<T extends Term> implements ToTermConverter<Enumeration<?>, T>, FromTermConverter<T, Enumeration<?>> {

	@Override
	public T toTerm(Enumeration<?> en, TypeDomain target, Jpc context) {
		return (T) new IterableConverter().toTerm(Collections.list(en), target, context);
	}
	
	@Override
	public Enumeration<?> fromTerm(T listTerm, TypeDomain target, Jpc context) {
		if (!listTerm.isList()) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		Type elementType = null;
		try {
			elementType = TypeWrapper.wrap(target.getType()).as(Enumeration.class).getActualTypeArgumentsOrUpperBounds()[0]; //will throw an exception if the type is not compatible with Enumeration
		} catch(Exception e) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		
		Type listType = parameterizedType(new Type[]{elementType}, null, List.class);
		List collection = context.fromTerm(listTerm, listType);
		return Collections.enumeration(collection);
	}
	
}
