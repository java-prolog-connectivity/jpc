package org.jpc.util.reification;

import static java.util.Collections.emptyList;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.internal.reflection.BeansUtil2;
import org.jpc.internal.reflection.ReflectionUtil;

public class ReflectiveObject<T> {

	protected static <U> U applyMethod(Object receiver, Class<?> receiverClass, String methodName, List<? extends Object> args) {
		U applied;
		Class<?>[] argTypes = new Class[args.size()];			
		for(int i = 0; i<args.size(); i++) {
			argTypes[i] = args.get(i).getClass();
		}
		if(methodName.equals("new")) {
			Constructor<U> constructor = ReflectionUtil.<U>getMatchingAccessibleConstructor( (Class<U>) receiverClass, argTypes);
			try {
				applied = constructor.newInstance(args.toArray());
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		} else {
			Method method = ReflectionUtil.getMatchingAccessibleMethod(receiverClass, methodName, argTypes);
			if(method == null)
				throw new RuntimeException("No matching method: " + methodName + " with types: " + argTypes + " in class: " + receiverClass);
			try {
				applied = (U) method.invoke(receiver, args.toArray()); //receiver is ignored if the method is static.
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		}
		return applied;
	}
	

	private final T wrapped;
	
	public ReflectiveObject(T wrappedObject) {
		this.wrapped = wrappedObject;
	}

	public T getWrapped() {
		return wrapped;
	}
	
	protected T getTargetObject() {
		return wrapped;
	}
	
	protected Class<?> getTargetClass() {
		return wrapped.getClass();
	}
	
	public <U> U invoke(String methodName) {
		return invoke(methodName, emptyList());
	}
	
	public <U> U invoke(String methodName, List<? extends Object> args) {
		return applyMethod(getTargetObject(), getTargetClass(), methodName, args);
	}
	
	public Type getFieldType(String fieldName) {
		Field field;
		try {
			field = getTargetClass().getField(fieldName);
		} catch (NoSuchFieldException | SecurityException e) {
			throw new RuntimeException(e);
		}
		return field.getGenericType();
	}
	
	public <U> U getField(String fieldName) {
		Field field;
		try {
			field = getTargetClass().getField(fieldName);
			return (U)field.get(getTargetObject());
		} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}
	
	public void setField(String fieldName, Object fieldValue) {
		Field field;
		try {
			field = getTargetClass().getField(fieldName);
		} catch (NoSuchFieldException | SecurityException e) {
			throw new RuntimeException(e);
		}
		BeansUtil2.setField(getTargetObject(), field, fieldValue);
	}
	
	public void setField(Entry<String, Object> entry) {
		setField(entry.getKey(), entry.getValue());
	}
	
	public void setFields(Map<String, Object> map) {
		for(Entry<String, Object> entry : map.entrySet()) {
			setField(entry);
		}
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((wrapped == null) ? 0 : wrapped.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ReflectiveObject other = (ReflectiveObject) obj;
		if (wrapped == null) {
			if (other.wrapped != null)
				return false;
		} else if (!wrapped.equals(other.wrapped))
			return false;
		return true;
	}

}
