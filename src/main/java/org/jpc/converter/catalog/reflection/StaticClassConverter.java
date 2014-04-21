package org.jpc.converter.catalog.reflection;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.STATIC_CLASS_FUNCTOR_NAME;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.catalog.reflection.reification.ClassConverter;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.StaticClass;

public class StaticClassConverter implements ToTermConverter<StaticClass, Compound>, FromTermConverter<Compound, StaticClass> {
	
	@Override
	public StaticClass fromTerm(Compound term, Type targetType, Jpc jpc) {
		return new StaticClass(ClassConverter.getRawClass(term, jpc));
	}

	@Override
	public Compound toTerm(StaticClass staticClass, Class<Compound> termClass, Jpc jpc) {
		String[] dotSplitted = staticClass.getWrappedClass().getName().split("[.]");
		List<String> packageFragmentNames = new ArrayList<>(asList(dotSplitted));
		String classPart = packageFragmentNames.remove(packageFragmentNames.size() - 1);
		List<String> classFragmentNames = asList(classPart.split("[$]"));
		return jpc.toCompound(STATIC_CLASS_FUNCTOR_NAME, asList(packageFragmentNames, classFragmentNames));
	}
	
}
