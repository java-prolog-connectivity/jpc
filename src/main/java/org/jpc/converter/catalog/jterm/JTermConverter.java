package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Compound;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;

public class JTermConverter extends JpcConverter<Object, Compound> {

	@Override
	public Object fromTerm(Compound term, Type type, Jpc context) {
		JTermManager jTermManager = context.getJTermManager();
		Object resolved = jTermManager.resolve(term); //try with the context jTerm manager
		if(resolved == null)
			resolved = JTermUtil.getJTermManager().resolve(term); //try with the default jTerm manager
		if(resolved == null)
			throw new JpcConversionException();
		return resolved;
	}

}
