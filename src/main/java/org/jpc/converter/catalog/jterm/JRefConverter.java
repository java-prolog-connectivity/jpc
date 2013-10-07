package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Compound;

public class JRefConverter extends JpcConverter<Object, Compound> {

	@Override
	public Object fromTerm(Compound term, Type type, Jpc context) {
		Object resolved = context.getJTermManager().resolve(term);
		if(resolved == null)
			throw new JpcConversionException();
		return resolved;
	}

}
