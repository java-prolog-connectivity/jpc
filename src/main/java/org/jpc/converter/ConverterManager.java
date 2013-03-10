package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.catalog.NullConverter;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.minitoolbox.reflection.wrappertype.ArrayTypeWrapper;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ConverterManager {
	
	private static Logger logger = LoggerFactory.getLogger(ConverterManager.class);
	
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
		if(term instanceof Variable)
			return new NullConverter().fromTerm((Variable) term, type, context);
		
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);
		Type termType = context.getType(term);
		if(termType != null && typeWrapper.isWeakAssignableFrom(termType)) { //if the term type is more specific, use that type instead
			type = typeWrapper.asType(termType);
			typeWrapper = TypeWrapper.wrap(type);
		}
		for(JpcConverter converter : converters) {
			Type bestTypeForConverter = type;
			Type converterTargetType;
			try {
				converterTargetType = converter.getObjectTypeOrThrow();
			} catch(Exception e) {
				logger.error("Converter error: " + e.toString());
				continue;
			}
			
			TypeWrapper converterTargetTypeWrapper = TypeWrapper.wrap(converterTargetType); //a type wrapper over the target type (the destination type) of the converter

			if(converterTargetTypeWrapper instanceof ArrayTypeWrapper) {//the converter converts to an array
				if(typeWrapper.isWeakAssignableFrom(converterTargetType)) {
					Type baseTypeArray = TypeWrapper.wrap(converterTargetTypeWrapper.getBaseType()).asType(typeWrapper.getBaseType());
					int typeArrayDimension = typeWrapper.getDimension();
					int converterArrayDimension = converterTargetTypeWrapper.getDimension();
					int dimension = typeArrayDimension<converterArrayDimension?converterArrayDimension:typeArrayDimension;
					bestTypeForConverter = ArrayTypeWrapper.createArray(baseTypeArray, dimension);
				} else
					continue;
			} else if(typeWrapper instanceof ArrayTypeWrapper) { //the current converter does not convert to arrays, but the desired type is an array
				continue;
			} else if(converter.objectTypeIsAssignableTo(type)) {
				bestTypeForConverter = typeWrapper.asType(converterTargetType);
			} else if (converter.objectTypeIsAssignableFrom(type)) {
				bestTypeForConverter = converterTargetTypeWrapper.asType(type);
			} else
				continue;
			//typeWrapper = TypeWrapper.wrap(type); 
			if(converter.canConvertFromTerm(term, bestTypeForConverter)) {
				try {
					return converter.fromTerm(term, bestTypeForConverter, context);
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion from term to objects
						JpcConversionException e){//converters should throw this exception if they are not able to convert a term to a Java object. Any other exception will be propagated to the caller
					//just try with the next converter if this exception occurs
				}
			}
		}
		throw new JpcConversionException(term.toString(), type.toString()); //no converter was able to convert the term to the desired type
	}
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass, Jpc context) {
		if(object==null) {
			if(termClass.isAssignableFrom(Variable.class))
				return (T) new NullConverter().toTerm(object, context);
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + termClass);
		}
			
		if(object instanceof Term)
			return (T) object; // a cast exception if the object is already a term, which is not compatible with the term class sent as argument
		if(object instanceof TermConvertable)
			return (T) ((TermConvertable)object).asTerm();
		

		for(JpcConverter converter : converters) {
			if(converter.canConvertToTerm(object, termClass)) {
				try {
					Class bestTermClassForConverter;
					Class converterClass = converter.getTermClassOrThrow();
					if(termClass.isAssignableFrom(converterClass))
						bestTermClassForConverter = converterClass;
					else
						bestTermClassForConverter = termClass;
					return (T) converter.toTerm(object, bestTermClassForConverter, context);
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion to terms
						JpcConversionException e) { //exception thrown if the converter finds that it is unable to convert certain object to a term
					//just try the next converter if any of these exceptions occurs
				} 
			}
		}
		throw new JpcConversionException(object.toString(), Term.class.getName());
	}

}
