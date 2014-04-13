package org.jpc.converter.catalog.reflection;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.StaticClass;

import com.google.common.base.Joiner;
import com.google.common.reflect.TypeToken;

import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.*;

public class StaticClassConverter implements ToTermConverter<StaticClass, Compound>, FromTermConverter<Compound, StaticClass> {
	
	private static String className(Iterable<String> packageFragmentNames, Iterable<String> classFragmentNames) {
		StringBuilder sb = new StringBuilder(Joiner.on('.').join(packageFragmentNames));
		if(sb.length() > 0)
			sb.append('.');
		sb.append(Joiner.on('$').join(classFragmentNames));
		return sb.toString();
	}
	
	@Override
	public StaticClass fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term packageFragmentsTerm = term.arg(1);
		Term classFragmentsTerm = term.arg(2);
		Type listStringsType = new TypeToken<List<String>>(){}.getType();
		List<String> packageFragmentNames = jpc.convert(packageFragmentsTerm, listStringsType);
		List<String> classFragmentNames = jpc.convert(classFragmentsTerm, listStringsType);
		return new StaticClass(className(packageFragmentNames, classFragmentNames));
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
