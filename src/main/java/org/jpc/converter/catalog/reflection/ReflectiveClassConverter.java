package org.jpc.converter.catalog.reflection;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.CLASS_FUNCTOR_NAME;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.catalog.reflection.reification.ClassConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.minitoolbox.reflection.ReflectiveClass;

public class ReflectiveClassConverter implements ToTermConverter<ReflectiveClass<?>, Compound>, FromTermConverter<Compound, ReflectiveClass<?>> {
	
	@Override
	public ReflectiveClass<?> fromTerm(Compound term, Type targetType, Jpc jpc) {
		return new ReflectiveClass<>(ClassConverter.getRawClass(term, jpc));
	}

	@Override
	public Compound toTerm(ReflectiveClass<?> staticClass, Class<Compound> termClass, Jpc jpc) {
		String[] dotSplitted = staticClass.getWrappedClass().getName().split("[.]");
		List<String> packageFragmentNames = new ArrayList<>(asList(dotSplitted));
		String classPart = packageFragmentNames.remove(packageFragmentNames.size() - 1);
		List<String> classFragmentNames = asList(classPart.split("[$]"));
		return jpc.toCompound(CLASS_FUNCTOR_NAME, asList(packageFragmentNames, classFragmentNames));
	}
	
	
	public static class ShortNotationReflectiveClassConverter implements FromTermConverter<Compound, ReflectiveClass<?>> {

		@Override
		public ReflectiveClass<?> fromTerm(Compound term, Type targetType, Jpc jpc) {
			Atom classNameTerm = (Atom) term.arg(1);
			return new ReflectiveClass<>(classNameTerm.getName());
		}
		
	}

}
