package org.jpc.engine.prolog.driver;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.jpc.engine.prolog.PrologEngine;

public class PrologEngineFactoryMethod<T extends PrologEngine> implements PrologEngineFactory<T> {

	private final Method factoryMethod;
	private final Object targetObject;
	
	/**
	 * 
	 * @param factoryMethod a static factory method.
	 */
	public PrologEngineFactoryMethod(Method factoryMethod) {
		this(factoryMethod, null);
	}
	
	/**
	 * 
	 * @param factoryMethod a factory method.
	 * @param targetObject the target object declaring the factory method. It may be null if the method is static.
	 */
	public PrologEngineFactoryMethod(Method factoryMethod, Object targetObject) {
		this.factoryMethod = factoryMethod;
		this.targetObject = targetObject;
	}
	
	@Override
	public T createPrologEngine() {
		try {
			return (T) factoryMethod.invoke(targetObject);
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}

}
