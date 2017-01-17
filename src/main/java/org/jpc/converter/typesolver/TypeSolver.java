package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

/**
 * Recommends the best conversion target type to a source object.
 * @author sergioc
 *
 */
public interface TypeSolver<T> {
	
	/**
	 * 
	 * @param object the object which conversion target type to recommend.
	 * @return the recommended type.
	 */
	Type inferType(T object);
	
}
