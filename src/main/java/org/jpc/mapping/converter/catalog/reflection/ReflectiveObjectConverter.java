package org.jpc.mapping.converter.catalog.reflection;

import static java.util.Arrays.asList;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.util.reification.ReflectiveObject;

public class ReflectiveObjectConverter implements ToTermConverter<ReflectiveObject, Compound>, FromTermConverter<Compound, ReflectiveObject> {

	public static final String REFLECTIVE_OBJECT_FUNCTOR_NAME = "robject";
	
	@Override
	public ReflectiveObject fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		Object object = jpc.fromTerm(term.arg(1));
		/*
		if(object instanceof ReflectiveClass)
			return (ReflectiveClass<?>) object;
		else
			return new ReflectiveObject(object);
		*/
		return new ReflectiveObject(object);
	}

	@Override
	public Compound toTerm(ReflectiveObject reflectiveObject, TypeDomain target, Jpc jpc) {
		Term term = jpc.toTerm(reflectiveObject.getWrapped());
		return new Compound(REFLECTIVE_OBJECT_FUNCTOR_NAME, asList(term));
	}

}
