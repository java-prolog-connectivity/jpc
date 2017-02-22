package org.jpc.converter.catalog.reflection;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import java.lang.reflect.Array;
import java.util.List;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.engine.prolog.PrologConstants;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Integer;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.util.PrologSpeakingClass;
import org.jpc.util.PrologSpeakingObject;
import org.jpc.util.reification.ReflectiveClass;

public class MethodCallConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		if (!term.hasFunctor(LogtalkConstants.LOGTALK_OPERATOR, 2)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		Term receiverTerm = term.arg(1);
		Term messageTerm = term.arg(2);
		List<Term> receivers = ListTerm.fromTermSequence(receiverTerm);
		Object result = null;
		for(Term aReceiver : receivers) {
			result = invoke(aReceiver, messageTerm, jpc); //broadcast the message to all the receivers.
		}
		return (T)result; //returns the last result.
	}
	
	private static Object invoke(Term receiverTerm, Term messageTerm, Jpc jpc) {
		if(messageTerm.hasFunctor(PrologConstants.SEQUENCE_SEPARATOR, 2)) {
			ListTerm cascadeMessages = ListTerm.fromTermSequence(messageTerm);
			return cascade(receiverTerm, cascadeMessages, jpc);
		} else {
			Object receiver = jpc.fromTerm(receiverTerm);
			PrologSpeakingObject prologSpeakingObject;
			if(receiver instanceof ReflectiveClass) {
				prologSpeakingObject = new PrologSpeakingClass(((ReflectiveClass<?>)receiver).getWrapped(), jpc);
			} else {
				prologSpeakingObject = new PrologSpeakingObject(receiver, jpc);
			}
			if(!messageTerm.isList()) {
				return prologSpeakingObject.invoke(messageTerm);
			} else {
				ListTerm listTerm = messageTerm.asList();
				if(listTerm.size() == 1) {
					if(listTerm.get(0) instanceof Atom) {
						return prologSpeakingObject.getField((Atom)listTerm.get(0));
					} else if(listTerm.get(0) instanceof Integer) {
						int index = ((Integer)listTerm.get(0)).intValue();
						if(receiver instanceof List) {
							return ((List)receiver).get(index);
						} else if(receiver.getClass().isArray()) {
							return Array.get(receiver, index);
						} else
							throw new JpcException("Index: " + index + " cannot be resolved for receiver: " + receiver + ".");
					} else {
						throw new JpcException("Wrong field specifier: " + listTerm.get(0));
					}
				} else if(listTerm.size() == 2) {
					if(listTerm.get(0) instanceof Atom) {
						Term fieldTerm = listTerm.get(1);
						prologSpeakingObject.setField((Atom)listTerm.get(0), fieldTerm);
					} else if(listTerm.get(0) instanceof Integer) {
						int index = ((Integer)listTerm.get(0)).intValue();
						Object value = jpc.fromTerm(listTerm.get(1));
						if(receiver instanceof List) {
							((List)receiver).set(index, value);
						} else if(receiver.getClass().isArray()) {
							Array.set(receiver, index, value);
						} else
							throw new JpcException("Index: " + index + " cannot be resolved for receiver: " + receiver + ".");
					} else {
						throw new JpcException("Wrong field specifier: " + listTerm.get(0) + ".");
					}
					return receiver;
				} else
					throw new JpcException("Invalid message: " + listTerm + ".");
			}
		}
	}
	
	private static Object cascade(Term receiverTerm, List<Term> messages, Jpc jpc) {
		Object result = null;
		for(Term aMessage : messages) {
			result = invoke(receiverTerm, aMessage, jpc);
		}
		return result; //returns the last result.
	}

}
