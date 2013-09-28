package org.jpc.converter;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

import com.google.common.reflect.TypeToken;

public abstract class JpcConverter<ObjectType,TermType extends Term> {

	private Type objectType;
	private Type termType;
	
	public JpcConverter() {
		objectType = new TypeToken<ObjectType>(getClass()){}.getType();
		termType = new TypeToken<TermType>(getClass()){}.getType();
	}

	public Type getObjectType() {
		return objectType;
	}
	
	public Type getTermType() {
		return termType;
	}
	
	public <T extends Term> boolean canConvertToTerm(Object object, Class<T> termClass) {
		return TypeWrapper.wrap(getTermType()).isAssignableFrom(termClass) &&
				TypeWrapper.wrap(getObjectType()).isWeakAssignableFrom(object.getClass());
	}
	
	public boolean canConvertFromTerm(Term term, Type toType) {
		return TypeWrapper.wrap(getTermType()).isAssignableFrom(term.getClass()) && 
				TypeWrapper.wrap(getObjectType()).isWeakAssignableFrom(toType); //the desired type can be a subtype of the type declared by the converter
	}
	
	public ObjectType fromTerm(TermType term, Jpc context) {
		return fromTerm(term, getObjectType(), context);
	}
	
	public ObjectType fromTerm(TermType term, Type type, Jpc context) {
		throw new UnsupportedOperationException();
	}
	
	public TermType toTerm(ObjectType object, Jpc context) {
		return (TermType) toTerm(object, (Class)getTermType(), context);
	}
	
	public <T extends TermType> T toTerm(ObjectType object, Class<T> termClass, Jpc context) {
		throw new UnsupportedOperationException();
	}

}
