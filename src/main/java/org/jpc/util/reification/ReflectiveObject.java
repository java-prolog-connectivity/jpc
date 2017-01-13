package org.jpc.util.reification;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.internal.reflection.BeansUtil2;
import org.jpc.internal.reflection.ReflectionUtil;

public class ReflectiveObject {

	protected static <T> T applyMethod(Object receiver, Class<?> receiverClass, String methodName, List<? extends Object> args) {
		T applied;
		Class<?>[] argTypes = new Class[args.size()];			
		for(int i = 0; i<args.size(); i++) {
			argTypes[i] = args.get(i).getClass();
		}
		if(methodName.equals("new")) {
			Constructor<T> constructor = ReflectionUtil.<T>getMatchingAccessibleConstructor((Class<T>)receiverClass, argTypes);
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
				applied = (T) method.invoke(receiver, args.toArray()); //receiver is ignored if the method is static.
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		}
		return applied;
	}
	

	private final Object wrappedObject;
	
	public ReflectiveObject(Object wrappedObject) {
		this.wrappedObject = wrappedObject;
	}

	public Object getWrappedObject() {
		return wrappedObject;
	}
	
	protected Object getTargetObject() {
		return wrappedObject;
	}
	
	protected Class<?> getTargetClass() {
		return wrappedObject.getClass();
	}
	
	public <T> T invoke(String methodName) {
		return invoke(methodName, Collections.emptyList());
	}
	
	public <T> T invoke(String methodName, List<? extends Object> args) {
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
				+ ((wrappedObject == null) ? 0 : wrappedObject.hashCode());
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
		if (wrappedObject == null) {
			if (other.wrappedObject != null)
				return false;
		} else if (!wrappedObject.equals(other.wrappedObject))
			return false;
		return true;
	}

}
