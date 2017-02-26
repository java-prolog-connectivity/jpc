package org.jpc.mapping.converter.catalog.list;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;



public class IterableConverter<T extends Term> implements ToTermConverter<Iterable<?>, T>, FromTermConverter<T, Iterable<?>> {

	@Override
	public T  toTerm(Iterable<?> it, TypeDomain target, Jpc context) {
		return (T) new IteratorConverter().toTerm(it.iterator(), target, context);
	}

	@Override
	public Iterable<?> fromTerm(T listTerm, TypeDomain target, Jpc context) {
		if (!listTerm.isList()) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(target.getType());
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
