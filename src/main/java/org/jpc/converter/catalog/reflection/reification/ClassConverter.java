package org.jpc.converter.catalog.reflection.reification;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.ARRAY_FUNCTOR_NAME;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.TYPE_FUNCTOR_NAME;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectiveClass;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.SingleTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

import com.google.common.base.Joiner;

public class ClassConverter implements ToTermConverter<Class<?>, Compound>, FromTermConverter<Compound, Class<?>> {

	static Class<?> classForName(Iterable<String> packageFragmentNames, Iterable<String> classFragmentNames) {
		StringBuilder sb = new StringBuilder(Joiner.on('.').join(packageFragmentNames));
		if(sb.length() > 0)
			sb.append('.');
		sb.append(Joiner.on('$').join(classFragmentNames));
		return ReflectiveClass.classForName(sb.toString());
	}
	
	public static Class<?> getRawClass(Compound term, Jpc jpc) {
		Term packageFragmentsTerm = term.arg(1);
		Term classFragmentsTerm = term.arg(2);
		List<String> packageFragmentNames = jpc.fromTerm(packageFragmentsTerm);
		List<String> classFragmentNames = jpc.fromTerm(classFragmentsTerm);
		return classForName(packageFragmentNames, classFragmentNames);
	}
	
	
	@Override
	public Compound toTerm(Class<?> clazz, Class<Compound> termClass, Jpc jpc) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(clazz);
		if(typeWrapper instanceof SingleTypeWrapper) {
			return toTerm((SingleTypeWrapper)typeWrapper, jpc);
		} else {
			return toTerm((ArrayTypeWrapper) typeWrapper, jpc);
		}
	}

	static Compound toTerm(SingleTypeWrapper singleTypeWrapper, Jpc jpc) {
		ReflectiveClass<?> staticClass = new ReflectiveClass<>((Class<?>)singleTypeWrapper.getRawType());
		
		String[] dotSplitted = staticClass.getWrappedClass().getName().split("[.]");
		List<String> packageFragmentNames = new ArrayList<>(asList(dotSplitted));
		String classPart = packageFragmentNames.remove(packageFragmentNames.size() - 1);
		List<String> classFragmentNames = asList(classPart.split("[$]"));
		
		Term packageFragmentNamesTerm = jpc.toTerm(packageFragmentNames);
		Term classFragmentNamesTerm = jpc.toTerm(classFragmentNames);
		
		Type[] actualTypeArguments = singleTypeWrapper.getActualTypeArguments();
		if(actualTypeArguments.length == 0) {
			return new Compound(TYPE_FUNCTOR_NAME, asList(packageFragmentNamesTerm, classFragmentNamesTerm));
		} else {
			Term actualTypeArgumentsTerm = jpc.toTerm(actualTypeArguments);
			Term ownerTypeTerm = jpc.toTerm(singleTypeWrapper.getOwnerType());
			return new Compound(TYPE_FUNCTOR_NAME, asList(packageFragmentNamesTerm, classFragmentNamesTerm, actualTypeArgumentsTerm, ownerTypeTerm));
		}
	}
	
	static Compound toTerm(ArrayTypeWrapper arrayTypeWrapper, Jpc jpc) {
		Compound componentTypeTerm = jpc.toTerm(arrayTypeWrapper.getComponentType(), Compound.class);
		return new Compound(ARRAY_FUNCTOR_NAME, asList(componentTypeTerm));
	}
	

	@Override
	public Class<?> fromTerm(Compound term, Type targetType, Jpc jpc) {
		return getRawClass(term, jpc);
	}
	
}
