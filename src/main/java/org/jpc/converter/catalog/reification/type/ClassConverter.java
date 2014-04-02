package org.jpc.converter.catalog.reification.type;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reification.type.ReificationConstants.ARRAY_FUNCTOR_NAME;
import static org.jpc.converter.catalog.reification.type.ReificationConstants.TYPE_FUNCTOR_NAME;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.reification.StaticClass;
import org.minitoolbox.reflection.typewrapper.ArrayTypeWrapper;
import org.minitoolbox.reflection.typewrapper.SingleTypeWrapper;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class ClassConverter implements ToTermConverter<Class<?>, Compound>, FromTermConverter<Compound, Class<?>>{

	@Override
	public Compound toTerm(Class<?> clazz, Class<Compound> termClass, Jpc jpc) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(clazz);
		if(typeWrapper instanceof SingleTypeWrapper) {
			return toTerm((SingleTypeWrapper)typeWrapper, jpc);
		} else {
			return toTerm((ArrayTypeWrapper) typeWrapper, jpc);
		}
	}

	static Compound toTerm(ArrayTypeWrapper arrayTypeWrapper, Jpc jpc) {
		Compound componentTypeTerm = jpc.toTerm(arrayTypeWrapper.getComponentType(), Compound.class);
		return new Compound(ARRAY_FUNCTOR_NAME, asList(componentTypeTerm));
	}
	
	static Compound toTerm(SingleTypeWrapper singleTypeWrapper, Jpc jpc) {
		StaticClass staticClass = new StaticClass((Class<?>)singleTypeWrapper.getRawType());
		Term rawClassTerm = jpc.toTerm(staticClass, Compound.class);
		
		Type[] actualTypeArguments = singleTypeWrapper.getActualTypeArguments();
		if(actualTypeArguments.length == 0) {
			return new Compound(TYPE_FUNCTOR_NAME, asList(rawClassTerm));
		} else {
			Term actualTypeArgumentsTerm = jpc.toTerm(actualTypeArguments);
			Term ownerTypeTerm = jpc.toTerm(singleTypeWrapper.getOwnerType());
			return new Compound(TYPE_FUNCTOR_NAME, asList(rawClassTerm, actualTypeArgumentsTerm, ownerTypeTerm));
		}
		
	}
	
	@Override
	public Class<?> fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term staticClassTerm = term.arg(1);
		StaticClass staticClass = jpc.fromTerm(staticClassTerm);
		return staticClass.getWrappedClass();
	}
	
}
