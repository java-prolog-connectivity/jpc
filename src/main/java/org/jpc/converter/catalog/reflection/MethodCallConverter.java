package org.jpc.converter.catalog.reflection;

import java.lang.reflect.Array;
import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.util.PrologSpeakingClass;
import org.jpc.util.PrologSpeakingObject;
import org.minitoolbox.reflection.StaticClass;

public class MethodCallConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		if(!term.hasFunctor(LogtalkConstants.LOGTALK_OPERATOR, 2))
			throw new ConversionException();
		Term receiverTerm = term.arg(1);
		Term messageTerm = term.arg(2);
		Object receiver = jpc.fromTerm(receiverTerm);
		PrologSpeakingObject prologSpeakingObject;
		if(receiver instanceof StaticClass) {
			prologSpeakingObject = new PrologSpeakingClass(((StaticClass)receiver).getWrappedClass(), jpc);
		} else {
			prologSpeakingObject = new PrologSpeakingObject(receiver, jpc);
		}
		if(!messageTerm.isList()) {
			return prologSpeakingObject.invoke(messageTerm);
		} else {
			ListTerm listTerm = messageTerm.asList();
			if(listTerm.size() == 1 && (listTerm.get(0) instanceof Atom || listTerm.get(0) instanceof IntegerTerm)) {
				if(listTerm.get(0) instanceof Atom) {
					String fieldName = ((Atom)listTerm.get(0)).getName();
					return prologSpeakingObject.getField(fieldName);
				} else {
					int index = ((IntegerTerm)listTerm.get(0)).intValue();
					if(receiver instanceof List) {
						return (T) ((List)receiver).get(index);
					} else if(receiver.getClass().isArray()) {
						return (T) Array.get(receiver, index);
					} else
						throw new JpcException("Index: " + index + " cannot be resolved for receiver: " + receiver);
				}
			} else {
				prologSpeakingObject.setFields(messageTerm);
				return (T) receiver;
			}
				
		}
			
	}

}
