package org.jpc.converter.instantiation;

import java.lang.reflect.Type;

import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;


public class SimpleInstanceCreator implements InstanceCreator {

	private Type type;
	private TypeWrapper typeWrapper;
	
	public SimpleInstanceCreator(Type type) {
		this.type = type;
		this.typeWrapper = TypeWrapper.wrap(type);
	}
	
	/**
	 * Instantiates the type received as argument.
	 * This type must be assignable to the type received in the constructor.
	 * This class just invokes the default constructor of the type. 
	 * Specialization classes could guide the instantiation according to type parameters (if any).
	 * @param type
	 * @return an instance of the type send as argument.
	 */
	@Override
	public <T> T instantiate(Type targetType) {
		TypeWrapper targetTypeWrapper = TypeWrapper.wrap(targetType);
		Type mostSpecificType = targetTypeWrapper.mostSpecificType(type);
		Class rawClass = TypeWrapper.wrap(mostSpecificType).getRawClass();
		T instantiation = null;
		try {
			instantiation = (T) rawClass.newInstance();
			return instantiation;
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}
	
	public boolean canInstantiate(Type targetType) {
		TypeWrapper targetTypeWrapper = TypeWrapper.wrap(targetType);
		try {
			targetTypeWrapper.mostSpecificType(type);
			return true;
		} catch(IncompatibleTypesException e) {
			return false;
		}
	}

}
