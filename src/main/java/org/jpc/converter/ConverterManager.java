package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.minitoolbox.reflection.wrappertype.ArrayTypeWrapper;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;
import org.minitoolbox.reflection.wrappertype.VariableTypeWrapper;

public class ConverterManager {

	private List<JpcConverter<?,?>> converters;
	
	public ConverterManager() {
		converters = new ArrayList<JpcConverter<?,?>>();
	}
	
	public void register(JpcConverter<?,?> converter) {
		converters.add(0, converter);
	}

	public Object fromTerm(Term term, Jpc context) {
		return fromTerm(term, Object.class, context);
	}

	public Object fromTerm(Term term, Type type, Jpc context) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);
		Type termType = context.getType(term);
		if(termType != null && typeWrapper.isWeakAssignableFrom(termType)) { //if the term type is more specific, use that type instead
			type = typeWrapper.asType(termType);
			typeWrapper = TypeWrapper.wrap(type);
		}
		for(JpcConverter converter : converters) {
			if(converter instanceof FromTermConverter) {
				Type bestTypeForConverter = type;
				FromTermConverter fromTermConverter = (FromTermConverter) converter;
				Type converterTargetType = fromTermConverter.getTargetType();
				TypeWrapper converterTargetTypeWrapper = TypeWrapper.wrap(fromTermConverter.getTargetType()); //a type wrapper over the target type (the destination type) of the converter
				
				
				
				
				if(converterTargetTypeWrapper instanceof ArrayTypeWrapper) {//the converter converts to an array
					
					if(typeWrapper.isWeakAssignableFrom(converterTargetType)) {
						Type baseTypeArray = TypeWrapper.wrap(converterTargetTypeWrapper.getBaseType()).asType(typeWrapper.getBaseType());
						int typeArrayDimension = typeWrapper.getDimension();
						int converterArrayDimension = converterTargetTypeWrapper.getDimension();
						int dimension = typeArrayDimension<converterArrayDimension?converterArrayDimension:typeArrayDimension;
						bestTypeForConverter = ArrayTypeWrapper.createArray(baseTypeArray, dimension);
					} else
						continue;
					
					
					/*
					if(type.equals(Object.class) || (typeWrapper instanceof VariableTypeWrapper)) { //the type requested is an Object, not an array. Anyway, all arrays extend from Object
						bestTypeForConverter = fromTermConverter.getTargetType(); //then use the target type (an array type) of the converter
					}
					else {
						try {
							bestTypeForConverter = converterTargetTypeWrapper.asType(type);
						} catch(Exception e) {
							continue; //try next converter
						}
					}
					*/
				} else if(typeWrapper instanceof ArrayTypeWrapper) { //the current converter does not convert to arrays, but the desired type is an array
					continue;
				} else if(fromTermConverter.targetTypeIsAssignableTo(type)) {
					bestTypeForConverter = typeWrapper.asType(converterTargetType);
				} else if (fromTermConverter.targetTypeIsAssignableFrom(type)) {
					bestTypeForConverter = converterTargetTypeWrapper.asType(type);
				} else
					continue;
				//typeWrapper = TypeWrapper.wrap(type); 
				if(fromTermConverter.canConvert(term, bestTypeForConverter)) {
					try {
						return fromTermConverter.convert(term, bestTypeForConverter, context);
					} catch(JpcConversionException e){//converters should throw this exception if they are not able to convert a term to a Java object. Any other exception will be propagated to the caller
						//just try with the next converter if this exception occurs
					}
				}
			}
		}
		throw new JpcConversionException(term.toString(), type.toString()); //no converter was able to convert the term to the desired type
	}
	
	public Term toTerm(Object object, Jpc context) {
		if(object==null)
			return Variable.ANONYMOUS_VAR;
		if(object instanceof TermConvertable)
			return ((TermConvertable)object).asTerm();
		
		for(JpcConverter converter : converters) {
			if(converter instanceof ToTermConverter) {
				ToTermConverter<Object> toTermConverter = (ToTermConverter) converter;
				if(toTermConverter.canConvert(object)) {
					try {
						return toTermConverter.convert(object, context);
					} catch(JpcConversionException e) { //this exception is raised if the converter find that it is unable to convert an object to a term
						//just try with the next converter if this exception occurs
					} 
					
				}
			}
		}
		throw new JpcConversionException(object.toString(), Term.class.getName());
	}

}
