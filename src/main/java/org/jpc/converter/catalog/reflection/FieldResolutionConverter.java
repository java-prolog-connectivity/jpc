package org.jpc.converter.catalog.reflection;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.StaticClass;

public class FieldResolutionConverter<T> implements FromTermConverter<Compound, T> {

	public static final String FIELD_RESOLUTION_OPERATOR = "@";
	
	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		if(!term.hasFunctor(FIELD_RESOLUTION_OPERATOR, 2))
			throw new ConversionException();
		
		Term receiverTerm = term.arg(1);
		Term fieldNameTerm = term.arg(2);
		
		Object receiver = jpc.fromTerm(receiverTerm);
		String fieldName = jpc.fromTerm(fieldNameTerm, String.class);
		
		Class<?> receiverClass;
		if(receiver instanceof StaticClass) {
			receiverClass = ((StaticClass)receiver).getWrappedClass();
		} else {
			receiverClass = receiver.getClass();
		}
		
		Field field;
		try {
			field = receiverClass.getField(fieldName);
			return (T) field.get(receiver);
		} catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
			throw new RuntimeException(e);
		}
		
	}

}
