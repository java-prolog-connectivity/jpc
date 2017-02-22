package org.jpc.converter.catalog.list;

import static java.util.Arrays.asList;
import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Array;
import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;
import org.typetools.typewrapper.ArrayTypeWrapper;
import org.typetools.typewrapper.TypeWrapper;


public class ArrayConverter<T, U extends Term> implements ToTermConverter<T[], U>, FromTermConverter<U, T[]> {

	@Override
	public U toTerm(T[] objects, TypeDomain target, Jpc context) {
		return new IteratorConverter<U>().toTerm(asList(objects).iterator(), target, context);
	}
	
	@Override
	public T[] fromTerm(Term listTerm, TypeDomain target, Jpc context) {
		if(!listTerm.isList()) {
			throw new DelegateConversionException(conversionGoal(listTerm, target));
		}
		TypeWrapper wrappedType = TypeWrapper.wrap(target.getType());
		ArrayTypeWrapper arrayTypeWrapper = (ArrayTypeWrapper) wrappedType;
		Type arrayComponentType = arrayTypeWrapper.getComponentType();
		TypeWrapper componentTypeWrapper = TypeWrapper.wrap(arrayComponentType);
		
		Type listType = parameterizedType(new Type[]{arrayComponentType}, null, List.class);
		List<T> list = (List) new CollectionConverter<>().fromTerm(listTerm, listType, context);
		T[] array = (T[]) Array.newInstance(componentTypeWrapper.getRawClass(), list.size());
		return (T[]) list.toArray(array);
	}
	
}
