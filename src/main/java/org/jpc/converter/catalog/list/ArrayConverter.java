package org.jpc.converter.catalog.list;

import static java.util.Arrays.asList;

import java.lang.reflect.Array;
import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class ArrayConverter<T, U extends Term> implements ToTermConverter<T[], U>, FromTermConverter<U, T[]> {

	@Override
	public U toTerm(T[] objects, Class<U> termClass, Jpc context) {
		return new IteratorConverter<U>().toTerm(asList(objects).iterator(), termClass, context);
	}
	
	@Override
	public T[] fromTerm(Term listTerm, Type targetType, Jpc context) {
		if(!listTerm.isList())
			throw new ConversionException();
		TypeWrapper wrappedType = TypeWrapper.wrap(targetType);
		ArrayTypeWrapper arrayTypeWrapper = (ArrayTypeWrapper) wrappedType;
		Type arrayComponentType = arrayTypeWrapper.getComponentType();
		TypeWrapper componentTypeWrapper = TypeWrapper.wrap(arrayComponentType);
		
		Type listType = new ParameterizedTypeImpl(new Type[]{arrayComponentType}, null, List.class);
		List list = (List)new CollectionConverter().fromTerm(listTerm, listType, context);
		T[] array = (T[]) Array.newInstance(componentTypeWrapper.getRawClass(), list.size());
		return (T[]) list.toArray(array);
	}
	
}