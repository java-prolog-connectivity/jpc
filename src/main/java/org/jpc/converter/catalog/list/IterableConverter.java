package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;


public class IterableConverter<T extends Term> implements BidirectionalTermConverter<Iterable, T> {

	@Override
	public T  toTerm(Iterable it, Class<T> termClass, Jpc context) {
		return (T) new IteratorConverter().toTerm(it.iterator(), termClass, context);
	}

	@Override
	public Iterable fromTerm(T term, Type targetType, Jpc context) {
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(targetType);
		Type componentType = null;
		TypeWrapper iterableTypeWrapper = wrappedTargetType.as(Iterable.class);
		if(iterableTypeWrapper.hasActualTypeArguments())
			componentType = iterableTypeWrapper.getActualTypeArguments()[0];
		else
			componentType = Object.class;
		
		Type listType = new ParameterizedTypeImpl(new Type[]{componentType}, null, List.class);
		return (List) new CollectionConverter().fromTerm(term, listType, context);
	}

}
