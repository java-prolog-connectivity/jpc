package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.catalog.NullConverter;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
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

	public Object fromTerm(Term term, Type type, Jpc context) {
		if(term instanceof Variable)
			return new NullConverter().fromTerm((Variable) term, type, context);

		Type termType = context.getType(term);
		if(termType != null) {
			try {
				type = TypeWrapper.wrap(termType).mostSpecificType(type); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {} //do nothing
		}		
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);

		for(JpcConverter converter : converters) {
			Type bestTypeForConverter;
			Type converterObjectType = converter.getObjectType();
			TypeWrapper converterObjectTypeWrapper = TypeWrapper.wrap(converterObjectType); //a type wrapper over the target type (the destination type) of the converter
		
			try {
				bestTypeForConverter = converterObjectTypeWrapper.mostSpecificType(type); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {
				continue;
			} //do nothing

			TypeWrapper bestTypeForConverterWrapper = TypeWrapper.wrap(bestTypeForConverter);
			
			if(converter.canConvertFromTerm(term, bestTypeForConverter)) {
				try {
					if(bestTypeForConverter.equals(converterObjectType)) { //this check is because in the call to fromTerm/2 the type arguments would be lost, then a call to fromTerm/3 would be more appropriate.
						return converter.fromTerm(term, context); //by default will delegate to fromTerm/3 unless overridding by the programmer
					} else {
						return converter.fromTerm(term, bestTypeForConverter, context); //the idea of these two calls is to allow the programmer to override either fromTerm/2 (if the type argument is not necessary) or fromTerm/3 (more verbose)
					}
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
				return (T) new NullConverter().toTerm(object, Variable.class, context);
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + termClass);
		}
			
		if(object instanceof Term)
			return (T) object; // a cast exception if the object is already a term, which is not compatible with the term class sent as argument
		if(object instanceof TermConvertable)
			return (T) ((TermConvertable)object).asTerm();
		

		for(JpcConverter converter : converters) {
			Class bestTermClassForConverter;
			Type converterTermType = converter.getTermType();
			TypeWrapper converterTermTypeWrapper = TypeWrapper.wrap(converterTermType);
			if(TypeWrapper.wrap(termClass).isAssignableFrom(converterTermType))
				bestTermClassForConverter = (Class) converterTermType;
			else if(converterTermTypeWrapper.isAssignableFrom(termClass))
				bestTermClassForConverter = termClass;
			else
				continue;
			if(converter.canConvertToTerm(object, bestTermClassForConverter)) {
				try {
					if(bestTermClassForConverter.equals(converterTermType)) {
						return (T) converter.toTerm(object, context);
					} else {
						return (T) converter.toTerm(object, bestTermClassForConverter, context);
					}
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion to terms
						JpcConversionException e) { //exception thrown if the converter finds that it is unable to convert certain object to a term
					//just try the next converter if any of these exceptions occurs
				} 
			}
		}
		throw new JpcConversionException(object.toString(), Term.class.getName());
	}

}
