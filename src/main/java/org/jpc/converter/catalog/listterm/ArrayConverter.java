package org.jpc.converter.catalog.listterm;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class ArrayConverter<E> extends JpcConverter<E[], Term> {

	@Override
	public <T extends Term> T toTerm(E[] objects, Class<T> termClass, Jpc context) {
		return new IterableConverter<E>().toTerm(asList(objects), termClass, context);
	}
	
	@Override
	public E[] fromTerm(Term term, Type type, Jpc context) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);
		if(!( (typeWrapper instanceof ArrayTypeWrapper) && TypeWrapper.wrap(getObjectType()).isWeakAssignableFrom(type) ))
			throw new JpcConversionException();
		
		ArrayTypeWrapper arrayTypeWrapper = (ArrayTypeWrapper) typeWrapper;
		Type arrayComponentType = arrayTypeWrapper.getComponentType();
		Type listType = new ParameterizedTypeImpl(new Type[]{arrayComponentType}, null, List.class);
		List<E> list = context.fromTerm(term, listType);

		E[] array = (E[]) ReflectionUtil.newArray(arrayComponentType, list.size());
		for(int i=0; i<list.size(); i++) {
			array[i] = list.get(i);
		}
		return array;
	}
	
}
