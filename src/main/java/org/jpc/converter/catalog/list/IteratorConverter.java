package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class IteratorConverter<T extends Term> implements ToTermConverter<Iterator<?>, T>, FromTermConverter<T, Iterator<?>> {

	@Override
	public T toTerm(Iterator<?> it, Class<T> termClass, Jpc context) {
		if(!Term.class.isAssignableFrom(termClass))
			throw new ConversionException();
		ListTerm terms = new ListTerm();
		while(it.hasNext())
			terms.add(context.toTerm(it.next()));
		return (T) terms.asTerm();
	}

	@Override
	public Iterator<?> fromTerm(T listTerm, Type targetType, Jpc context) {
		if(!listTerm.isList())
			throw new ConversionException();
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(targetType);
		Type componentType = null;
		TypeWrapper iteratorTypeWrapper = wrappedTargetType.as(Iterator.class);
		if(iteratorTypeWrapper.hasActualTypeArguments())
			componentType = iteratorTypeWrapper.getActualTypeArguments()[0];
		else
			componentType = Object.class;
		
		Type listType = new ParameterizedTypeImpl(new Type[]{componentType}, null, List.class);
		
		List list = (List) new CollectionConverter().fromTerm(listTerm, listType, context);
		return list.iterator();
	}

}
