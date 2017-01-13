package org.jpc.internal.reflection;

import static com.google.common.collect.Sets.newHashSet;
import static org.jconverter.util.typevisitor.TypeVisitor.InterfaceMode.EXCLUDE_INTERFACES;
import static org.jconverter.util.typevisitor.TypeVisitor.InterfaceMode.INCLUDE_INTERFACES;

import java.io.File;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.lang3.reflect.ConstructorUtils;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.jconverter.internal.reification.GenericArrayTypeImpl;
import org.jconverter.internal.reification.ParameterizedTypeImpl;
import org.jconverter.internal.reification.TypeVariableImpl;
import org.jconverter.util.IncompatibleTypesException;
import org.jconverter.util.typevisitor.FindFirstTypeVisitor;
import org.jconverter.util.typevisitor.TypeVisitor;
import org.jconverter.util.typewrapper.TypeWrapper;
import org.reflections.ReflectionUtils;
import org.reflections.Reflections;
import org.reflections.scanners.ResourcesScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;
import org.reflections.util.FilterBuilder;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.reflect.TypeToken;


public class ReflectionUtil {

	public static boolean isFileLoaded(File file, URLClassLoader classLoader) {
		try {
			return isUrlLoaded(file.toURI().toURL(), classLoader);
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		}
	}

	public static boolean isUrlLoaded(URL url, URLClassLoader classLoader) {
		for(URL urlClassLoader : classLoader.getURLs()) {
			if(urlClassLoader.equals(url))
				return true;
		}
		return false;
	}

	/**
	 * This method answers a set of URLs pointing to the 'effective' classpath entries of an array of classloaders
	 * This set includes both any URL entry in the hierarchy of classloaders, together with classpath entries declared in manifest files located in any jar in the classpath
	 * @param classLoaders the queried classloaders
	 * @return Returns a set of URLs pointing to the effective classpath entries of an array of classloaders
	 */
	public static Set<URL> effectiveClassPathUrls(ClassLoader... classLoaders) {
		return newHashSet(ClasspathHelper.forManifest(ClasspathHelper.forClassLoader(classLoaders)));
	}

	/**
	 * Answers whether a resource is in a given package
	 * @param resource the name of the resource
	 * @param packageName the name of the package
	 * @return whether the resouce is in a package
	 */
	public static boolean isResourceInPackage(String resource, String packageName) {
		return isResourceInPackage(resource, packageName, true);
	}

	/**
	 * Answers whether a resource is in a given package
	 * For example, resource 'p1/p2/r' is in package p1.p2
	 * @param resource the name of the resource
	 * @param packageName the name of the package
	 * @param includeSubPackages whether subpackages should be ignored
	 * @return whether the resouce is in a package
	 */
	public static boolean isResourceInPackage(String resource, String packageName, boolean includeSubPackages) {
		String packageAsPath = packageName!=null?packageName.replace(".", "/"):"";
		if(resource.startsWith(packageAsPath)) {
			if(!includeSubPackages) {
				String subPath = resource.substring(packageAsPath.length()); //path relative to the given package
				if(subPath.startsWith("/"))
					subPath = subPath.substring(1);
				if(subPath.contains("/"))
					return false;
			}
			return true;
		} else
			return false;
	}

	public static Set<String> findResourcesInPackage(String packageName, Predicate<String> predicate, ClassLoader... classLoaders) {
		return findResourcesInPackage(packageName, predicate, true, classLoaders);
	}

	/**
	 * Answers a set of resources that satisfy a given predicate and that are located in a given package
	 * @param packageName the name of the package where the resources are located
	 * @param predicate the predicate that should be satisfied by any found resource
	 * @param includeSubPackages whether resources located in subpackages should be taken into account
	 * @param classLoaders the classloaders that loaded the resources
	 * @return a set with all the resources satisfying the predicate
	 */
	public static Set<String> findResourcesInPackage(final String packageName, Predicate<String> predicate, final boolean includeSubPackages, ClassLoader... classLoaders) {
		Set<URL> urls = effectiveClassPathUrls(classLoaders);
		ConfigurationBuilder cb = new ConfigurationBuilder();
		cb.setUrls(urls);
		cb.setScanners(new ResourcesScanner());
		FilterBuilder fb = new FilterBuilder();
		fb.include(FilterBuilder.prefix(packageName));
		cb.filterInputsBy(fb);
		Reflections reflections = new Reflections(cb);
		Set<String> resources = reflections.getResources(predicate);
		if(!includeSubPackages) {
			Predicate<String> isResourceInPackage = new Predicate<String>() {
				@Override
				public boolean apply(String resource) {
					return isResourceInPackage(resource, packageName, includeSubPackages);
				}
			};
			resources = new HashSet<>(Collections2.filter(resources, isResourceInPackage));
		}
		return resources;
	}

	/**
	 * Answers a String representing the package where the resource is located. An empty String if the resource is in the default package
	 * @param resource the resource to inspect
	 * @return a String representing the package where the resource is located. An empty String if the resource is in the default package
	 */
	public static String resourcePackage(String resource) {
		String[] splitted = resource.split("/");
		String fileResourceName = splitted[splitted.length-1];
		String parentPackage = resource.substring(0, resource.length() - fileResourceName.length());
		parentPackage = parentPackage.replace("/", ".");
		return parentPackage;
	}

	public static Set<String> resourcesWithAnyExtension(String resource, ClassLoader... classLoaders) {
		String packageName = resourcePackage(resource);
		String resourceName = resource.substring(packageName.length());
		return resourcesWithAnyExtension(resourceName, packageName, false, classLoaders);
	}

	/**
	 * Answers a set of resources with a given name (with an arbitrary extension) and located in a certain package
	 * @param resourceName the name of the resource to find
	 * @param packageName the package where the resource is located
	 * @param includeSubPackages if packages should be included in the search
	 * @param classLoaders the classloaders that loaded the resources
	 * @return a set of resources with the given name plus their extension
	 */
	public static Set<String> resourcesWithAnyExtension(final String resourceName, String packageName, boolean includeSubPackages, ClassLoader... classLoaders) {
		Predicate<String> hasName = new Predicate<String>() {
			@Override
			public boolean apply(String name) {
				if(name.equals(resourceName) || name.matches(Pattern.quote(resourceName)+"\\..+"))
					return true;
				else
					return false;
			}
		};
		return findResourcesInPackage(packageName, hasName, includeSubPackages, classLoaders);
	}

	/**
	 * Answers an ordered set with the URLs of a resource with a given name in given classLoaders
	 * @param resource the resource to find
	 * @param classLoaders the classloaders where to look for the resource
	 * @return an ordered set with all the resources with the given name
	 */
	public static Set<URL> getResources(String resource, ClassLoader... classLoaders) {
		Set<URL> allResources = new LinkedHashSet<>(); //LinkedHashSet to preserve the insertion order
		ClassLoader[] validClassLoaders = ClasspathHelper.classLoaders(classLoaders);
		for(ClassLoader cl : validClassLoaders) {
			try {
				Enumeration<URL> urls = cl.getResources(resource);
				while(urls.hasMoreElements()) {
					URL url = urls.nextElement();
					allResources.add(url);
				}
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		return allResources;
	}

	/**
	 * Answers the URL of the class in the first different classpath than the caller in the stack
	 * @return the URL (classpath) of the client library
	 */
	public static URL getConsumerLibraryUrl() {
		try {
			//The first element in the stack trace is the getStackTrace method, the second is this method, the third is the method which requires to know the classpath of the caller
			StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
			Class<?> callerClass = Class.forName(stackTraceElements[2].getClassName()); //the class calling this method can be found at the third element in the stack trace
			URL callerUrl = ClasspathHelper.forClass(callerClass); //the classpath url of the caller

			//we start looking for a different url at the fourth member
			for(int i = 3; i<Thread.currentThread().getStackTrace().length; i++) {
				StackTraceElement stackTraceElement = Thread.currentThread().getStackTrace()[i];

				Class<?> candidateClass = Class.forName(stackTraceElement.getClassName());
				URL candidateUrl = ClasspathHelper.forClass(candidateClass);
				//for runtime generated classes the URL will be null.
				if(candidateUrl != null && !candidateUrl.equals(callerUrl))
					return candidateUrl; //found a caller in the stack trace located in a different library (a different classpath url)
			}
			return null;
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Answers if the two methods can handle the same message
	 * This is true if they have the same name and same number and type of parameters
	 * (the return type is not relevant)
	 * @param m1 a method
	 * @param m2 a method
	 * @return if the two methods have the same argument types and number
	 */
	public static boolean handleSameMessage(Method m1, Method m2) {
		/*
		if(!m1.getReturnType().equals(m2.getReturnType()))
			return false;
		*/
		if(!m1.getName().equals(m2.getName()))
			return false;

		Class<?>[] params1 = m1.getParameterTypes();
		Class<?>[] params2 = m2.getParameterTypes();
		if(params1.length != params2.length)
			return false;

		for(int i = 0; i<params1.length; i++) {
			if(!params1[i].equals(params2[i]))
				return false;
		}
		return true;
	}

	/**
	 *
	 * @param method
	 * @param methods
	 * @return true if the first parameter method handle the same message than one method in the second parameter collection, false otherwise
	 */
	private static boolean isHandled(Method method, Iterable<Method> methods) {
		for(Method m : methods) {
			if(handleSameMessage(method, m))
				return true;
		}
		return false;
	}


	public static <T> Constructor<T> getMatchingAccessibleConstructor(Class<T> clazz, Class<?> ...paramClasses) {
		return ConstructorUtils.getMatchingAccessibleConstructor(clazz, paramClasses);
	}

	public static Method getMatchingAccessibleMethod(Class<?> clazz, String methodName, Class<?> ...paramClasses) {
		return MethodUtils.getMatchingAccessibleMethod(clazz, methodName, paramClasses);
	}

	public static List<Method> getAllAbstractMethods(Class<?> clazz) {
		List<Method> publicAbstractMethods = new ArrayList<>();
		getAllPublicAbstractMethods(clazz, publicAbstractMethods);
		List<Method> nonPublicAbstractMethods = new ArrayList<>();
		getAllNonPublicAbstractMethods(clazz, nonPublicAbstractMethods);
		List<Method> allAbstractMethods = publicAbstractMethods;
		allAbstractMethods.addAll(nonPublicAbstractMethods);
		return allAbstractMethods;
	}

	private static void getAllPublicAbstractMethods(Class<?> clazz, List<Method> abstractMethods) {
		if(clazz == null)
			return;
		for(Method method : clazz.getMethods()) //only answers public methods (both declared and inherited). It includes any method declared in the class interfaces (methods in interfaces must be public)
			if(isAbstract(method) && !isHandled(method, abstractMethods))
				abstractMethods.add(method);
		getAllNonPublicAbstractMethods(clazz.getSuperclass(), abstractMethods);
	}

	private static void getAllNonPublicAbstractMethods(Class<?> clazz, List<Method> abstractMethods) {
		if(clazz == null)
			return;
		for(Method method : clazz.getDeclaredMethods()) //answers ALL the methods declared by the class. Methods in the class interfaces are ignored.
			if(!isPublic(method) && isAbstract(method) && !isHandled(method, abstractMethods))
				abstractMethods.add(method);
		getAllNonPublicAbstractMethods(clazz.getSuperclass(), abstractMethods);
	}




	/**
	 *
	 * @param clazz a class
	 * @param interfaze an interface
	 * @return a boolean indicating if a class adds an interface to its class hierarchy
	 */
	public static boolean includesInterfaceInHierarchy(Class<?> clazz, Class<?> interfaze) {
		//Object.class will never answer true to the first condition, so the call to getSuperclass() in the second is safe
		return (interfaze.isAssignableFrom(clazz) && !interfaze.isAssignableFrom(clazz.getSuperclass()));
	}

	/**
	 *
	 * @param clazz a class
	 * @return an array with all the interfaces included by {@code clazz}
	 */
	public static Class<?>[] includedInterfaces(Class<?> clazz) {
		List<Class<?>> includedInterfaces = new ArrayList<>();
		for(Class<?> interfaze : clazz.getInterfaces()) {
			if(includesInterfaceInHierarchy(clazz, interfaze))
				includedInterfaces.add(interfaze);
		}
		return includedInterfaces.toArray(new Class[] {});
	}


	/**
	 * @param ancestor the ancestor in the hierarchy
	 * @param descendant the descendant in the hierarchy
	 * @return All the classes between {@code ancestor} and {@code descendant} ({@code ancestor} and {@code descendant} are also included)
	 * @throws IncompatibleTypesException in case {@code ancestor} is not an ancestor of {@code descendant}
	 */
	public static Class<?>[] getClassesInHierarchy(Class<?> ancestor, Class<?> descendant) {
		List<Class<?>> hierarchy = new ArrayList<>();

		Class<?> currentDescendant = descendant;
		while(true) {
			hierarchy.add(0, currentDescendant);
			if(currentDescendant.equals(ancestor)) { //done, we reach the ancestor in the hierarchy
				return hierarchy.toArray(new Class[] {});
			} else if(currentDescendant.equals(Object.class)) {
				throw new IncompatibleTypesException(ancestor, descendant);
			} else {
				currentDescendant = currentDescendant.getSuperclass();
			}
		}
	}

	/*
	public static Field getField(Object target, String propertyName) {
		return getFieldInHierarchy(target.getClass(), propertyName);
	}

	private static Field getFieldInHierarchy(Class clazz, String propertyName) {
		Field field = null;
		try {
			field = clazz.getField(propertyName); //does not work for non public fields
		} catch(NoSuchFieldException e1) { //Unknown property
			try {
				field = getFieldInHierarchyAux(clazz, propertyName);
			} catch(Exception e2) {
				throw new RuntimeException(e1);
			}
		}
		return field;
	}

	private static Field getFieldInHierarchyAux(Class clazz, String propertyName) {
		Field field = null;
		try {
			field = clazz.getDeclaredField(propertyName); //all the fields declared by the current class
		} catch(NoSuchFieldException e2) { //Unknown property
			if(clazz.equals(Object.class))
				throw new RuntimeException(e2);
			else
				field = getFieldInHierarchyAux(clazz.getSuperclass(), propertyName);
		}
		return field;
	}
*/


	public static boolean isVisible(Class<?> clazz, Field field) {
		Field visibleField = getVisibleField(clazz, field.getName());
		return visibleField != null && visibleField.equals(field);
	}

	public static Field getVisibleField(Class<?> clazz, String fieldName) {
		return visibleFields(clazz).get(fieldName);
	}

	/**
	 * Returns a map with all the visible fields in a class:
	 * - all the fields declared in the class,
	 * - the public and protected fields of the ancestor classes, and
	 * - the "package" fields of superclasses located in the same package
	 * @param clazz a class
	 * @return the visible fields of a class
	 */
	public static Map<String, Field> visibleFields(Class<?> clazz) {
		Map<String, Field> visibleFields = new HashMap<String, Field>();
		Field[] declaredFields = clazz.getDeclaredFields();
		for(Field declaredField : declaredFields) {
			visibleFields.put(declaredField.getName(), declaredField);
		}
		visibleSuperFields(clazz, visibleFields);
		return visibleFields;
	}

	public static void visibleSuperFields(final Class<?> clazz, final Map<String, Field> visibleFields) {
		Class<?> superClass = clazz.getSuperclass();
		if(superClass != null) {
			TypeVisitor typeVisitor = new TypeVisitor(EXCLUDE_INTERFACES) {
				@Override public boolean doVisit(Class<?> clazzInHierarchy) {
					Field[] declaredFields = clazzInHierarchy.getDeclaredFields();
					for(Field declaredField : declaredFields) {
						if(!visibleFields.containsKey(declaredField.getName())) //check if the field is already there
							if(!Modifier.isPrivate(declaredField.getModifiers())) { //exclude private fields in super classes
								if(!hasPackageAccessModifier(declaredField) || clazzInHierarchy.getPackage().equals(clazz.getPackage())) //exclude 'package' fields in classes declared in different packages
									visibleFields.put(declaredField.getName(), declaredField);
							}
					}
					return true;
				}
			};
			typeVisitor.visit(superClass);
		}
	}

	public static boolean hasPackageAccessModifier(int modifiers) {
		return !Modifier.isPrivate(modifiers) && !Modifier.isProtected(modifiers) && !Modifier.isPublic(modifiers);
	}

	public static boolean isAbstract(Type type) {
		return Modifier.isAbstract(TypeToken.of(type).getRawType().getModifiers()); //primitive type classes answer yes to this
	}

	public static boolean isInterface(Type type) {
		return TypeToken.of(type).getRawType().isInterface();
	}

	public static boolean isAbstract(Method method) {
		return Modifier.isAbstract(method.getModifiers());
	}

	public static boolean isPublic(Method method) {
		return Modifier.isPublic(method.getModifiers());
	}

	public static boolean isProtected(Method method) {
		return Modifier.isProtected(method.getModifiers());
	}

	public static boolean isPrivate(Method method) {
		return Modifier.isPrivate(method.getModifiers());
	}

	public static boolean hasPackageAccessModifier(Method method) {
		return hasPackageAccessModifier(method.getModifiers());
	}

	public static boolean isPublic(Field field) {
		return Modifier.isPublic(field.getModifiers());
	}

	public static boolean isProtected(Field field) {
		return Modifier.isProtected(field.getModifiers());
	}

	public static boolean isPrivate(Field field) {
		return Modifier.isPrivate(field.getModifiers());
	}

	public static boolean isFloatingPoint(Number number) {
		return number instanceof BigDecimal || number instanceof Float || number instanceof Double;
	}

	public static boolean hasPackageAccessModifier(Field field) {
		return hasPackageAccessModifier(field.getModifiers());
	}


	public static <A extends Annotation> A getParameterAnnotation(Method method, int position, Class<A> annotationClass) {
		for(Annotation anAnnotation : method.getParameterAnnotations()[position]) {
			if(anAnnotation.annotationType().equals(annotationClass))
				return (A) anAnnotation;
		}
		return null;
	}

	public static Class<?> findFirstNonSyntheticClass(Class<?> candidateClass) {
		FindFirstTypeVisitor finderVisitor = new FindFirstTypeVisitor(INCLUDE_INTERFACES) {
			@Override
			public boolean match(Class<?> clazz) {
				return !clazz.isSynthetic();
			}
		};
		finderVisitor.visit(candidateClass);
		return finderVisitor.getFoundType();
	}

	/**
	 * Unchecked instantiation of a class
	 * @param clazz the class to instantiate
	 * @return the instantiated object
	 */
	public static <T> T newInstance(Class<T> clazz) {
		try {
			return clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	public static Object newArray(Type type, int length) {
		return Array.newInstance(TypeWrapper.wrap(type).getRawClass(), length);
	}

	public static boolean instanceOfOne(Object o, Class<?>... classes) {
		for(Class<?> clazz : classes)
			if(clazz.isAssignableFrom(o.getClass()))
				return true;
		return false;
	}

	public static <T> Set<Class<? extends T>> filterAbstractClasses(Set<Class<? extends T>> unfilteredClasses) {
		Predicate<Class<?>> predicate = new Predicate<Class<?>>() {
			public boolean apply(Class<?> clazz) {
				return !isAbstract(clazz);
			}
		};
		return ReflectionUtils.getAll(unfilteredClasses, predicate);
	}

	public static ParameterizedType parameterizedType(Type[] actualTypeArguments, Type ownerType, Class<?> rawType) {
		return new ParameterizedTypeImpl(actualTypeArguments, ownerType, rawType);
	}

	public static GenericArrayType genericArrayType(Type genericComponentType) {
		return new GenericArrayTypeImpl(genericComponentType);
	}

	public static <D extends GenericDeclaration> TypeVariable<D> typeVariable(D genericDeclaration, String name, Type[] upperBounds) {
		return new TypeVariableImpl<>(genericDeclaration, name, upperBounds);
	}

}
