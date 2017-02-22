package org.jpc.converter.catalog.error;

import static java.util.Arrays.asList;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Integer;

public class StackTraceElementConverter implements ToTermConverter<StackTraceElement, Compound> {

	public static final String STACK_TRACE_ELEMENT_FUNCTOR_NAME = "stk";
	
	@Override
	public Compound toTerm(StackTraceElement stk, TypeDomain target, Jpc jpc) {
		return new Compound(STACK_TRACE_ELEMENT_FUNCTOR_NAME, asList(new Atom(stk.getClassName()), new Atom(stk.getMethodName()), new Integer(stk.getLineNumber())));
	}

}
