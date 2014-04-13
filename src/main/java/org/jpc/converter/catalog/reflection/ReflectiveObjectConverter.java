package org.jpc.converter.catalog.reflection;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectiveObject;

public class ReflectiveObjectConverter implements ToTermConverter<ReflectiveObject, Compound>, FromTermConverter<Compound, ReflectiveObject> {

	public static final String REFLECTIVE_OBJECT_FUNCTOR_NAME = "reflective";
	
	@Override
	public ReflectiveObject fromTerm(Compound term, Type targetType, Jpc jpc) {
		Object object = jpc.fromTerm(term.arg(1));
		return new ReflectiveObject(object);
	}

	@Override
	public Compound toTerm(ReflectiveObject reflectiveObject, Class<Compound> termClass, Jpc jpc) {
		Term term = jpc.toTerm(reflectiveObject.getWrappedObject());
		return new Compound(REFLECTIVE_OBJECT_FUNCTOR_NAME, asList(term));
	}

}
