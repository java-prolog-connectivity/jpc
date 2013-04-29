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
	
//	public Type getPreferredObjectType() {
//		return asNonVariableType(objectType);
//	}
//	
//	public Class<TermType> getPreferredTermClass() {
//		return (Class<TermType>) asNonVariableType(termType);
//	}
//
//	private Type asNonVariableType(Type type) {
//		Type nonVariableType;
//		if(type instanceof VariableTypeWrapper || 
//				(type instanceof ArrayTypeWrapper && ((ArrayTypeWrapper)type).getBaseType() instanceof VariableTypeWrapper)) {
//			nonVariableType = TypeWrapper.wrap(type).getRawClass();
//		} else
//			nonVariableType = type;
//		return nonVariableType;
//	}
	
	
	
//	public Type getObjectTypeOrThrow() {
//		TypeWrapper typeWrapper = TypeWrapper.wrap(objectType);
//		if(typeWrapper instanceof VariableTypeWrapper)
//			throw new RuntimeException("The converter object type is not available.");
//		return objectType;
//	}
//	
//	public Class getTermClassOrThrow() {
//		TypeWrapper typeWrapper = TypeWrapper.wrap(termType);
//		if(typeWrapper instanceof VariableTypeWrapper)
//			throw new RuntimeException("The converter term type not available.");
//		return typeWrapper.getRawClass();
//	}
//
//	public boolean termClassIsEquals(Type type) {
//		return getTermClassOrThrow().equals(type);
//	}
//	
//	public boolean termClassIsAssignableTo(Type type) {
//		return TypeWrapper.wrap(type).isWeakAssignableFrom(getTermClassOrThrow());
//	}
//
//	public boolean termClassIsAssignableFrom(Type type) {
//		return TypeWrapper.wrap(getTermClassOrThrow()).isWeakAssignableFrom(type);
//	}
//	
//	public boolean objectTypeIsEquals(Type type) {
//		return getObjectTypeOrThrow().equals(type);
//	}
//	
//	public boolean objectTypeIsAssignableTo(Type type) {
//		return TypeWrapper.wrap(type).isWeakAssignableFrom(getObjectTypeOrThrow());
//	}
//
//	public boolean objectTypeIsAssignableFrom(Type type) {
//		return TypeWrapper.wrap(getObjectTypeOrThrow()).isWeakAssignableFrom(type);
//	}
	
	
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
	
	public <T extends TermType>T toTerm(ObjectType object, Class<T> termClass, Jpc context) {
		throw new UnsupportedOperationException();
	}

}
