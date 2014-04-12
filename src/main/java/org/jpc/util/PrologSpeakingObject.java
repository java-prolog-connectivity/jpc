package org.jpc.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.BeansUtil;
import org.minitoolbox.reflection.ReflectionUtil;

public class PrologSpeakingObject {

	protected static <T> T applyMethod(Object receiver, Class<?> receiverClass, String methodName, List<Term> argTerms, Jpc jpc) {
		T applied;
		Object[] args = new Object[argTerms.size()];
		Class<?>[] argTypes = new Class[argTerms.size()];			
		for(int i = 0; i<argTerms.size(); i++) {
			Object objectArg = jpc.fromTerm(argTerms.get(i));
			args[i] = objectArg;
			argTypes[i] = objectArg.getClass();
		}
		if(methodName.equals("new")) {
			Constructor<T> constructor = ReflectionUtil.<T>getMatchingAccessibleConstructor((Class<T>)receiverClass, argTypes);
			try {
				applied = constructor.newInstance(args);
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		} else {
			Method method = ReflectionUtil.getMatchingAccessibleMethod(receiverClass, methodName, argTypes);
			if(method == null)
				throw new JpcException("No mathing method: " + methodName + " with types: " + argTypes + " in class: " + receiverClass);
			try {
				applied = (T) method.invoke(receiver, args); //receiver is ignored if the method is static.
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		}
		return applied;
	}
	
	
	private final Object wrappedObject;
	private final Jpc jpc;
	
	public PrologSpeakingObject(Object wrappedObject, Jpc jpc) {
		this.wrappedObject = wrappedObject;
		this.jpc = jpc;
	}

	public Object getWrappedObject() {
		return wrappedObject;
	}

	public Jpc getJpcContext() {
		return jpc;
	}
	
	protected Object getTargetObject() {
		return wrappedObject;
	}
	
	protected Class<?> getTargetClass() {
		return wrappedObject.getClass();
	}
	
	private <T> T invoke(Atom atom) {
		return invoke(atom.getName(), Collections.emptyList());
	}
	
	private <T> T invoke(Compound compound) {
		return invoke(compound.getNameString(), compound.getArgs());
	}
	
	public <T> T invoke(Term messageTerm) {
		if(messageTerm instanceof Atom) {
			return invoke((Atom)messageTerm);
		} else if(messageTerm instanceof Compound) {
			return invoke((Compound)messageTerm);
		} else
			throw new JpcException("Unsupported message term type: " + messageTerm);
	}
	
	public <T> T invoke(String methodName, List<Term> args) {
		return applyMethod(getTargetObject(), getTargetClass(), methodName, args, jpc);
	}
	
	public void setField(String fieldName, Term fieldValueTerm) {
		Object fieldValue = jpc.fromTerm(fieldValueTerm);
		Field field;
		try {
			field = getTargetClass().getField(fieldName);
		} catch (NoSuchFieldException | SecurityException e) {
			throw new RuntimeException(e);
		}
		BeansUtil.setField(getTargetObject(), field, fieldValue);
	}
	
}
