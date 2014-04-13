package org.jpc.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectiveObject;

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
		return reflectiveObject.getWrappedObject();
	}

	public Jpc getJpcContext() {
		return jpc;
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
	
	public <T> T invoke(String methodName, List<? extends Term> argTerms) {
		List<Object> args = new ArrayList<>();		
		for(int i = 0; i<argTerms.size(); i++) {
			args.add(jpc.fromTerm(argTerms.get(i)));
		}
		return reflectiveObject.invoke(methodName, args);
	}

	public void setField(String fieldName, Term fieldValueTerm) {
		Object fieldValue = jpc.fromTerm(fieldValueTerm);
		reflectiveObject.setField(fieldName, fieldValue);
	}
	
}
