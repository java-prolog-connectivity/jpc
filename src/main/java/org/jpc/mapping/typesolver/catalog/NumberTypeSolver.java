package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jconverter.typesolver.TypeSolver;
import org.jpc.term.Float;
import org.jpc.term.Integer;

public class NumberTypeSolver implements TypeSolver<Number> {

	@Override
	public Type inferType(Number number) {
		Class numberClass = number.getClass();
		if(numberClass.equals(Long.class) || numberClass.equals(java.lang.Integer.class) || numberClass.equals(Short.class) || numberClass.equals(Byte.class) ||
				numberClass.equals(BigInteger.class) || numberClass.equals(AtomicInteger.class) || numberClass.equals(AtomicLong.class)) {
			return Integer.class;
		} else
			return Float.class;
	}

}
