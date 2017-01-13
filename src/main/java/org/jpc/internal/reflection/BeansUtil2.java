package org.jpc.internal.reflection;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;


/**
 * In the spirit of Apache bean utils. 
 * Most methods here are not there (probably it needs some cleaning ...)
 * 
 * @author scastro
 *
 */
public class BeansUtil2 {


	private static final String GETTER_PREFIX_NON_BOOLEAN = "get";
	private static final String GETTER_PREFIX_BOOLEAN = "is";
	private static final String SETTER_PREFIX_NON_BOOLEAN = "set";
	
	public static boolean looksLikeGetter(Method method) {
		String name = method.getName();
		return ( method.getParameterTypes().length == 0 && (name.startsWith(GETTER_PREFIX_NON_BOOLEAN) || name.startsWith(GETTER_PREFIX_BOOLEAN)) );
	}
	
	public static boolean looksLikeSetter(Method method) {
		String name = method.getName();
		return (method.getParameterTypes().length == 1 && name.startsWith(SETTER_PREFIX_NON_BOOLEAN));
	}
	
	public static boolean looksLikeBeanMethod(Method method) {
		return looksLikeGetter(method) || looksLikeSetter(method);
	}

	public static String getterName(String propertyName, Class clazz) {
		if(clazz.equals(Boolean.class) || clazz.equals(boolean.class))
			return booleanGetterName(propertyName);
		else
			return nonBooleanGetterName(propertyName);
	}
	
	public static String nonBooleanGetterName(String propertyName) {
		return GETTER_PREFIX_NON_BOOLEAN + propertyName.substring(0,1).toUpperCase() + propertyName.substring(1);
	}
	
	public static String booleanGetterName(String propertyName) {
		return GETTER_PREFIX_BOOLEAN + propertyName.substring(0,1).toUpperCase() + propertyName.substring(1);
	}
	
	public static String setterName(String propertyName) {
		return SETTER_PREFIX_NON_BOOLEAN + propertyName.substring(0,1).toUpperCase() + propertyName.substring(1);
	}
	
	
	
	
	
	
	
	/**
	 * Given a property name, answers the first getter in a class hierarchy
	 * @param clazz a class
	 * @param propertyName a property in the class
	 * @return the first getter in a class hierarchy
	 */
	public static Method getterInHierarchy(Class clazz, String propertyName) {
		return getterInHierarchy(clazz, propertyName, null);
	}
	
	public static Method getterInHierarchy(Class clazz, String propertyName, Class getterClass) {
		Method method = publicGetter(clazz, propertyName);
		if(method == null)
			method = getterInHierarchyAux(clazz, propertyName, getterClass);
		return method;
	}
	
	public static Method getterInHierarchyAux(Class clazz, String propertyName, Class getterClass) {
		if(clazz == null)
			return null;
		Method method = declaredGetter(clazz, propertyName, getterClass);
		if(method != null)
			return method;
		else
			return getterInHierarchyAux(clazz.getSuperclass(), propertyName, getterClass);
	}
	
	/**
	 * Given a property name, answers the first setter in a class hierarchy
	 * Note that multiple setters could exist in a class for the same property name
	 * this method will answer the first setter found
	 * @param clazz a class
	 * @param propertyName a property in the class
	 * @return the first setter in a class hierarchy
	 */
	public static Method setterInHierarchy(Class clazz, String propertyName) {
		return setterInHierarchy(clazz, propertyName, null);
	}
	
	public static Method setterInHierarchy(Class clazz, String propertyName, Class setterClass) {
		Method method = publicSetter(clazz, propertyName);
		if(method == null)
			method = setterInHierarchyAux(clazz, propertyName, setterClass);
		return method;
	}
	
	public static Method setterInHierarchyAux(Class clazz, String propertyName, Class setterClass) {
		if(clazz == null)
			return null;
		Method method = declaredSetter(clazz, propertyName, setterClass);
		if(method != null)
			return method;
		else
			return setterInHierarchyAux(clazz.getSuperclass(), propertyName, setterClass);
	}
	

	
	
	public static Method declaredGetter(Class clazz, String propertyName, Class getterClass) {
		Method getter = null;
		if(getterClass != null)
			try {
				getter = clazz.getDeclaredMethod(getterName(propertyName, getterClass));
			} catch (NoSuchMethodException e2) {
			}
		else {
			try {
				getter = clazz.getDeclaredMethod(nonBooleanGetterName(propertyName));
			} catch (NoSuchMethodException e) {
				try {
					getter = clazz.getDeclaredMethod(booleanGetterName(propertyName));
					if(! (getter.getReturnType().equals(Boolean.class) || getter.getReturnType().equals(boolean.class)) )
						getter = null;
				} catch (NoSuchMethodException e1) {
				}
			}
		}
		return getter;
	}
	

	public static Method declaredSetter(Class clazz, String propertyName, Class setterClass) {
		Method setter = null;
		if(setterClass != null)
			try {
				setter = clazz.getDeclaredMethod(setterName(propertyName), setterClass);
			} catch (NoSuchMethodException e) {
			}
		else {
			String setterName = setterName(propertyName);
			for(Method method : clazz.getDeclaredMethods()) {
				if(method.getName().equals(setterName) && method.getParameterTypes().length == 1) {
					setter = method;
					break;
				}
			}
		}
		return setter;
	}
	
	public static Method publicGetter(Class clazz, String propertyName, Class getterClass) {
		Method getter = null;
		if(getterClass != null)
			try {
				getter = clazz.getMethod(getterName(propertyName, getterClass));
			} catch (NoSuchMethodException e2) {
			}
		else {
			try {
				getter = clazz.getMethod(nonBooleanGetterName(propertyName));
			} catch (NoSuchMethodException e) {
				try {
					getter = clazz.getMethod(booleanGetterName(propertyName));
					if(! (getter.getReturnType().equals(Boolean.class) || getter.getReturnType().equals(boolean.class)) )
						getter = null;
				} catch (NoSuchMethodException e1) {
				}
			}
		}
		return getter;
	}
	

	public static Method publicSetter(Class clazz, String propertyName, Class setterClass) {
		Method setter = null;
		if(setterClass != null)
			try {
				setter = clazz.getMethod(setterName(propertyName), setterClass);
			} catch (NoSuchMethodException e) {
			}
		else {
			String setterName = setterName(propertyName);
			for(Method method : clazz.getMethods()) {
				if(method.getName().equals(setterName) && method.getParameterTypes().length == 1) {
					setter = method;
					break;
				}
			}
		}
		return setter;
	}
	
	
	public static Method publicGetter(Class clazz, String propertyName) {
		return publicGetter(clazz, propertyName, null);
	}
	
	public static Method publicSetter(Class clazz, String propertyName) {
		return publicSetter(clazz, propertyName, null);
	}
	
	/*
	private static PropertyDescriptor getPropertyDescriptor(Class clazz, String propertyName) {
		for(PropertyDescriptor propertyDescriptor : PropertyUtils.getPropertyDescriptors(clazz)) {
			if(propertyDescriptor.getName().equals(propertyName))
				return propertyDescriptor;
		}
		return null;
	}
	*/
	
	public Type getPropertyType(Object target, String propertyName) {
		Type type = null;
		
		Method getter = publicGetter(target.getClass(), propertyName);
		if(getter != null)
			type = getter.getGenericReturnType();
		else {
			Method setter = publicSetter(target.getClass(), propertyName);
			if(setter != null)
				type = setter.getGenericParameterTypes()[0];
			else {
				Field field = ReflectionUtil.getVisibleField(target.getClass(), propertyName);
				if(field != null)
					type = field.getGenericType();
				else
					throw new RuntimeException("The property: " + propertyName + " does not exist in class " + target.getClass());
			}
		}
		return type;
	}
	

	private static Field getAccessibleField(String fieldName, Class definingClass) {
		Field field;
		try {
			field = definingClass.getDeclaredField(fieldName);
		} catch (NoSuchFieldException | SecurityException e) {
			throw new RuntimeException(e);
		}
		field.setAccessible(true); //this is necessary to make accessible non-public fields, otherwise an illegal access exception will be thrown
		return field;
	}

	public static Object getPropertyWithGetter(Object target, String propertyName) throws NoSuchMethodException {
		Method getter = publicGetter(target.getClass(), propertyName);
		if(getter == null)
			throw new NoSuchMethodException();
		try {
			return getter.invoke(target);
		} catch (IllegalAccessException | IllegalArgumentException
				| InvocationTargetException e1) {
			throw new RuntimeException(e1);
		}
	}
	
	public static void setPropertyWithSetter(Object target, String propertyName, Object value) throws NoSuchMethodException {
		Method setter = publicSetter(target.getClass(), propertyName);
		if(setter == null)
			throw new NoSuchMethodException();
		try {
			setter.invoke(target, value);
		} catch (IllegalAccessException | IllegalArgumentException
				| InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}
	

	public static Object getField(Object target, Field field) {
		try {
			field.setAccessible(true);
			return field.get(target);
		} catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
			throw new RuntimeException(e);
		} 
	}
	
	public static void setField(Object target, Field field, Object value) {
		try {
			field.setAccessible(true);
			field.set(target, value);
		} catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
			throw new RuntimeException(e);
		} 
	}
	
	public static Object getField(Object target, String propertyName, Class definingClass) {
		try {
			Field field = getAccessibleField(propertyName, definingClass);
			return getField(target, field);
		} catch (SecurityException | IllegalArgumentException e) {
			throw new RuntimeException(e);
		} 
	}
	
	public static void setField(Object target, String propertyName, Class definingClass, Object value) {
		try {
			Field field = getAccessibleField(propertyName, definingClass);
			setField(target, field, value);
		} catch (SecurityException | IllegalArgumentException e) {
			throw new RuntimeException(e);
		} 
	}
	
	public static Object getProperty(Object target, String propertyName, Class definingClass) {
		Object value = null;
		try {
			value = getPropertyWithGetter(target, propertyName); //try with an accessor if possible
		} catch(Exception e) { //getter no defined
			if(definingClass == null) {
				Field visibleField = ReflectionUtil.getVisibleField(target.getClass(), propertyName);
				if(visibleField != null)
					value = getField(target, visibleField);
				else
					throw new RuntimeException(e);
			} else {
				value = getField(target, propertyName, definingClass); //try obtaining directly the field
			}
			
		}
		return value;
	}
	
	public static Object getProperty(Object target, String propertyName) {
		return getProperty(target, propertyName, null);
	}
	
	public static List getProperties(Object target, List<String> propertiesNames, Class definingClass) {
		List propertiesValues = new ArrayList<>();
		for(String propertyName: propertiesNames)
			propertiesValues.add(getProperty(target, propertyName, definingClass));
		return propertiesValues;
	}
	
	public static List getProperties(Object target, List<String> propertiesNames) {
		return getProperties(target, propertiesNames, null);
	}
	
	public static void setProperty(Object target, String propertyName, Object value, Class definingClass) {
		try {
			setPropertyWithSetter(target, propertyName, value);
		} catch (Exception e) { //setter no defined
			if(definingClass == null) {
				Field visibleField = ReflectionUtil.getVisibleField(target.getClass(), propertyName);
				if(visibleField != null)
					setField(target, visibleField, value);
				else
					throw new RuntimeException(e);
			}
			setField(target, propertyName, definingClass, value); //try obtaining directly the field
		} 
	}
	
	public static void setProperty(Object target, String propertyName, Object value) {
		setProperty(target, propertyName, value, null);
	}
	
	public static void setProperties(Object target, List<String> propertiesNames, List propertiesValues, Class definingClass) {
		for(int i=0; i<propertiesNames.size(); i++)
			setProperty(target, propertiesNames.get(i), propertiesValues.get(i), definingClass);
	}
	
	public static void setProperties(Object target, List<String> propertiesNames, List propertiesValues) {
		setProperties(target, propertiesNames, propertiesValues, null);
	}
}
