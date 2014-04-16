package org.jpc.converter.catalog.reflection;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.term.Compound;
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
			prologSpeakingObject.setFields(messageTerm);
			return (T) receiver;
		}
			
	}

}
