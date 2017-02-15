package org.jpc.util;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.util.reification.ReflectiveObject;

import com.google.common.reflect.TypeToken;

public class PrologSpeakingObject {
	
	private final ReflectiveObject reflectiveObject;
	private final Jpc jpc;
	
	public PrologSpeakingObject(Object object, Jpc jpc) {
		this(new ReflectiveObject(object), jpc);
	}
	
	protected PrologSpeakingObject(ReflectiveObject reflectiveObject, Jpc jpc) {
		this.reflectiveObject = reflectiveObject;
		this.jpc = jpc;
	}

	public Object getWrappedObject() {
		return reflectiveObject.getWrapped();
	}

	public Jpc getJpcContext() {
		return jpc;
	}
	
	private <T> T invoke(Atom atom) {
		return invoke(atom, Collections.emptyList());
	}
	
	private <T> T invoke(Compound compound) {
		return invoke((Atom)compound.getNameTerm(), compound.getArgs());
	}
	
	public <T> T invoke(Term messageTerm) {
		if(messageTerm instanceof Atom) {
			return invoke((Atom)messageTerm);
		} else if(messageTerm instanceof Compound) {
			return invoke((Compound)messageTerm);
		} else
			throw new JpcException("Unsupported message term type: " + messageTerm);
	}
	
	public <T> T invoke(Atom methodName, List<? extends Term> argTerms) {
		return invoke(methodName.getName(), argTerms);
	}
	
	public <T> T invoke(String methodName, List<? extends Term> argTerms) {
		List<Object> args = new ArrayList<>();		
		for(int i = 0; i<argTerms.size(); i++) {
			args.add(jpc.fromTerm(argTerms.get(i)));
		}
		return (T) reflectiveObject.invoke(methodName, args);
	}

	public void setField(Atom fieldName, Term fieldValueTerm) {
		Type fieldType = reflectiveObject.getFieldType(fieldName.getName());
		Object fieldValue = jpc.fromTerm(fieldValueTerm, fieldType);
		reflectiveObject.setField(fieldName.getName(), fieldValue);
	}
	
	public void setFields(Term mapTerm) {
		Type mapType = new TypeToken<Map<Atom,Term>>(){}.getType();
		Map<Atom,Term> map = jpc.fromTerm(mapTerm, mapType);
		for(Entry<Atom, Term> entry : map.entrySet()) {
			setField(entry.getKey(), entry.getValue());
		}
	}
	
	public <T> T getField(Atom fieldName) {
		return (T) reflectiveObject.getField(fieldName.getName());
	}
	
}
