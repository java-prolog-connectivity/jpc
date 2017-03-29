package org.jpc.mapping.converter.catalog.collection;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jconverter.converter.TypeDomain.typeDomain;
import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;


public class IteratorConverter<T extends Term> implements ToTermConverter<Iterator<?>, T>, FromTermConverter<T, Iterator<?>> {

	@Override
	public T toTerm(Iterator<?> it, TypeDomain target, Jpc context) {
		if (!target.isSubsetOf(typeDomain(Term.class))) {
			throw new DelegateConversionException(conversionGoal(it, target));
		}
		ListTerm terms = new ListTerm();
		while (it.hasNext()) {
			terms.add(context.toTerm(it.next()));
		}
		return (T) terms.asTerm();
	}

	@Override
	public Iterator<?> fromTerm(T listTerm, TypeDomain target, Jpc context) {
		if (!listTerm.isList()) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(target.getType());
		Type componentType = null;
		TypeWrapper iteratorTypeWrapper = wrappedTargetType.as(Iterator.class);
		if (iteratorTypeWrapper.hasActualTypeArguments()) {
			componentType = iteratorTypeWrapper.getActualTypeArguments()[0];
		} else {
			componentType = Object.class;
		}

		Type listType = parameterizedType(new Type[]{componentType}, null, List.class);
		
		List list = (List) new CollectionConverter().fromTerm(listTerm, listType, context);
		return list.iterator();
	}

}
