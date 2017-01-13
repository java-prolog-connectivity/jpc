package org.jpc.util.reification;

import java.util.List;

import org.apache.commons.lang3.ClassUtils;


/**
 * The reification of a static class declaration in Java.
 * @author sergioc
 *
 */
public class ReflectiveClass<T> extends ReflectiveObject {
	
	public static <T> Class<T> classForName(String className) {
		try {
			return (Class<T>)ClassUtils.getClass(className); //works well with primitive types.
			//return Class.forName(className); //does not work with primitive types.
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
	}
	
	public ReflectiveClass(String className) {
		this(classForName(className));
	}
	
	public ReflectiveClass(Class<T> wrappedClass) {
		super(wrappedClass);
	}

	public Class<T> getWrappedClass() {
		return (Class<T>) getWrappedObject();
	}
	
	@Override
	protected Object getTargetObject() {
		return null;
	}
	
	@Override
	protected Class<T> getTargetClass() {
		return getWrappedClass();
	}
	
	public T newInstance(List<? extends Object> args) {
		return invoke("new", args);
	}
	
}
