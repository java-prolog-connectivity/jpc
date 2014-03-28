package org.jpc.converter.catalog.logtalk;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;
import org.minitoolbox.reflection.reification.StaticClass;

public class LogtalkMethodCallConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		if(!term.hasFunctor(LogtalkConstants.LOGTALK_OPERATOR, 2))
			throw new ConversionException();
		Term receiverTerm = term.arg(1);
		Term messageTerm = term.arg(2);
		
		Object receiver = jpc.fromTerm(receiverTerm);
		
		String methodName;
		Object[] args;
		Class<?>[] argTypes;
		if(messageTerm instanceof Atom) {
			methodName = ((Atom)messageTerm).getName();
			args = new Object[]{};
			argTypes = new Class[]{};
		} else {
			methodName = ((Compound)messageTerm).getNameString();
			args = new Object[messageTerm.arity()];
			argTypes = new Class[messageTerm.arity()];
			for(int i = 0; i<messageTerm.arity(); i++) {
				Object objectArg = jpc.fromTerm(messageTerm.arg(i+1));
				args[i] = objectArg;
				argTypes[i] = objectArg.getClass();
			}
		}
		Class<?> receiverClass;
		if(receiver instanceof StaticClass) {
			receiverClass = ((StaticClass)receiver).getWrappedClass();
			if(methodName.equals("new")) {
				Constructor<T> constructor = ReflectionUtil.<T>getMatchingAccessibleConstructor((Class<T>)receiverClass, argTypes);
				try {
					return constructor.newInstance(args);
				} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
					throw new RuntimeException(e);
				}
			}
		} else {
			receiverClass = receiver.getClass();
		}

		Method method = ReflectionUtil.getMatchingAccessibleMethod(receiverClass, methodName, argTypes);
		if(method == null)
			throw new JpcException("No mathing method: " + methodName + " with types: " + argTypes + " in class: " + receiverClass);
		try {
			T returned = (T) method.invoke(receiver, args); //receiver is ignored if the method is static.
			return returned;
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}

}
