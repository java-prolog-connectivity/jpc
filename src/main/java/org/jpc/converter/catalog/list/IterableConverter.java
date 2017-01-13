package org.jpc.converter.catalog.list;

import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jconverter.util.typewrapper.TypeWrapper;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;



public class IterableConverter<T extends Term> implements ToTermConverter<Iterable<?>, T>, FromTermConverter<T, Iterable<?>> {

	@Override
	public T  toTerm(Iterable<?> it, Class<T> termClass, Jpc context) {
		return (T) new IteratorConverter().toTerm(it.iterator(), termClass, context);
	}

	@Override
	public Iterable<?> fromTerm(T listTerm, Type targetType, Jpc context) {
		if(!listTerm.isList())
			throw new ConversionException();
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(targetType);
		Type componentType = null;
		TypeWrapper iterableTypeWrapper = wrappedTargetType.as(Iterable.class);
		if(iterableTypeWrapper.hasActualTypeArguments())
			componentType = iterableTypeWrapper.getActualTypeArguments()[0];
		else
			componentType = Object.class;
		
		Type listType = parameterizedType(new Type[]{componentType}, null, List.class);
		return (List) new CollectionConverter().fromTerm(listTerm, listType, context);
	}

}
