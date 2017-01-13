package org.jpc.internal.collections;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import com.google.common.collect.Sets;

public class CollectionsUtil {

	/**
	 * There is currently no class in Java for a weak set, but it can be created as a set wrapping a weak map as in the body of the method below.
	 * @return a weak set
	 */
	public static <T> Set<T> createWeakSet() {
		return Collections.newSetFromMap(new WeakHashMap<T, Boolean>());
	}
	
	public static <T> Set<T> createWeakSet(Iterable<T> items) {
		Set<T> set = createWeakSet();
		set.addAll(Sets.newHashSet(items));
		return set;
	}
	
	public static boolean isKnownMultiValuedObject(Object object) {
		return isKnownMultiValuedClass(object.getClass());
	}

	public static boolean isKnownMultiValuedClass(Class clazz) {
		return( Map.class.isAssignableFrom(clazz) || Object[].class.isAssignableFrom(clazz) || Iterable.class.isAssignableFrom(clazz) || Iterator.class.isAssignableFrom(clazz) || Enumeration.class.isAssignableFrom(clazz) );
	}

}
