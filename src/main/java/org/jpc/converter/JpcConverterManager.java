package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import javassist.bytecode.SignatureAttribute.ObjectType;

import org.jconverter.converter.ConversionException;
import org.jconverter.converter.Converter;
import org.jpc.Jpc;
import org.jpc.term.Term;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

//TODO delete
public class JpcConverterManager {

	/*
	private static Logger logger = LoggerFactory.getLogger(JpcConverterManager.class);
	
	private List<Converter<?,?>> converters;
	
	public JpcConverterManager() {
		converters = new ArrayList<Converter<?,?>>();
	}
	
	public void register(Converter<?,?> converter) {
		converters.add(0, converter);
	}
	
	public void registerLast(Converter<?,?> converter) {
		converters.add(converter);
	}

	@Override
	public ObjectType fromTerm(TermType term, Type type, Jpc context) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);

		for(Converter converter : converters) {
			Type bestTypeForConverter;
			Type converterObjectType = converter.getObjectType();
		
			try {
				bestTypeForConverter = typeWrapper.mostSpecificType(converterObjectType); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {
				continue; //do nothing, just try the next converter
			} 

			if(converter.canConvertFromTerm(term, bestTypeForConverter)) {
				try {
					//this check is because in the call to fromTerm/2 the type argument would be lost. If the type is more specific than the converter type, a call to fromTerm/3 is more appropriate.
					if(bestTypeForConverter.equals(converterObjectType)) { 
						return (ObjectType) converter.fromTerm(term, context); //by default will delegate to fromTerm/3 unless overridden by the programmer
					} else {
						return (ObjectType) converter.fromTerm(term, bestTypeForConverter, context); //the idea of these two alternatives is to allow the programmer to override either fromTerm/2 (if the type argument is not necessary) or fromTerm/3 (more verbose)
					}
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion from term to objects
						ConversionException e){//converters should throw this exception if they are not able to convert a term to a Java object.
					//just try the next converter if any of these exceptions occurs
				}
			}
		}
		throw new ConversionException(term.toString(), type.toString()); //no converter was able to convert the term to the desired type
	}
	
	@Override
	public <T extends TermType> T toTerm(ObjectType object, Class<T> termClass, Jpc context) {
		TypeWrapper termClassWrapper = TypeWrapper.wrap(termClass);
		
		for(Converter converter : converters) {
			Class<? extends Term> bestTermClassForConverter;
			Type converterTermType = converter.getTermType();
			
			try {
				bestTermClassForConverter = (Class<? extends Term>)termClassWrapper.mostSpecificType(converterTermType); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {
				continue; //do nothing, just try the next converter
			}
			
			if(converter.canConvertToTerm(object, bestTermClassForConverter)) {
				try {
					if(bestTermClassForConverter.equals(converterTermType)) {
						return (T) converter.toTerm(object, context);
					} else {
						return (T) converter.toTerm(object, bestTermClassForConverter, context);
					}
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion to terms
						ConversionException e) { //exception thrown if the converter finds that it is unable to convert certain object to a term.
					//just try the next converter if any of these exceptions occurs
				} 
			}
		}
		throw new ConversionException(object.toString(), Term.class.getName());
	}
	*/
}
