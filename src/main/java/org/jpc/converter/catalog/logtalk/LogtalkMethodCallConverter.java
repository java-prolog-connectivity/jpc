package org.jpc.converter.catalog.logtalk;

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
				System.out.println(objectArg);
				System.out.println(objectArg.getClass());
				args[i] = objectArg;
				argTypes[i] = objectArg.getClass();
			}
		}
		Method method = ReflectionUtil.getMatchingAccessibleMethod(receiver.getClass(), methodName, argTypes);
		if(method == null)
			throw new JpcException("No mathing method: " + methodName + " with types: " + argTypes + " in class: " + receiver.getClass());
		try {
			return (T) method.invoke(receiver, args);
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}

}
