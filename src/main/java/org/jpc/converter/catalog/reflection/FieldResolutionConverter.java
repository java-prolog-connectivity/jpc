package org.jpc.converter.catalog.reflection;

import java.lang.reflect.Type;
import java.util.Map;
import java.util.Map.Entry;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectiveObject;
import org.minitoolbox.reflection.StaticClass;

public class FieldResolutionConverter<T> implements FromTermConverter<Compound, T> {

	public static final String FIELD_RESOLUTION_OPERATOR = "@";
	
	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		if(!term.hasFunctor(FIELD_RESOLUTION_OPERATOR, 2))
			throw new ConversionException();
		
		Term receiverTerm = term.arg(1);
		Term fieldSpecifierTerm = term.arg(2);
		
		Object receiver = jpc.fromTerm(receiverTerm);
		ReflectiveObject reflectiveObject;
		if(receiver instanceof StaticClass) {
			reflectiveObject =(StaticClass<?>) receiver;
		} else {
			reflectiveObject = new ReflectiveObject(receiver);
		}
		
		if(fieldSpecifierTerm instanceof Atom) {
			String fieldName = jpc.fromTerm(fieldSpecifierTerm, String.class);
			return reflectiveObject.getField(fieldName);
		} else {
			/*
			Object mutations = jpc.fromTerm(fieldSpecifierTerm);
			if(mutations instanceof Entry) {
				reflectiveObject.setField((Entry<String, Object>) mutations);
			} else if(mutations instanceof Map) {
				reflectiveObject.setFields((Map<String, Object>) mutations);
			} else*/
				throw new JpcException("Invalid field specifier.");
			
			//return (T) receiver;
		}
		
	}

}
