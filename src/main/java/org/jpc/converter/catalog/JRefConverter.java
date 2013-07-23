package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.jterm.JRef;
import org.jpc.jterm.RefId;
import org.jpc.jterm.RefManager;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;

public class JRefConverter extends JpcConverter<Object, Compound> {

	@Override
	public Object fromTerm(Compound term, Type type, Jpc context) {
		if(!term.hasFunctor(JRef.JREF_FUNCTOR, 1)) {
			throw new JpcConversionException();
		}
		IntegerTerm iTerm = (IntegerTerm) term.arg(1);
		RefId refId = new RefId(iTerm.intValue());
		return RefManager.getDefaultRefManager().getOrThrow(refId);
	}

}
