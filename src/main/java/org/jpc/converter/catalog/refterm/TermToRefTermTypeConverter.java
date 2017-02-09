package org.jpc.converter.catalog.refterm;

import static org.jpc.engine.prolog.ReturnSpecifierConstants.BB_REF_TERM_FLAG;
import static org.jpc.engine.prolog.ReturnSpecifierConstants.SOFT_REF_FLAG;
import static org.jpc.engine.prolog.ReturnSpecifierConstants.STRONG_REF_FLAG;
import static org.jpc.engine.prolog.ReturnSpecifierConstants.WB_REF_TERM_FLAG;
import static org.jpc.engine.prolog.ReturnSpecifierConstants.WEAK_REF_FLAG;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.internal.gc.ReferenceType;
import org.jpc.term.Compound;
import org.jpc.term.refterm.RefTermType;
import org.jpc.term.refterm.RefTermType.Opacity;

public class TermToRefTermTypeConverter implements FromTermConverter<Compound, RefTermType> {

	@Override
	public RefTermType fromTerm(Compound term, Type targetType, Jpc context) {
		ReferenceType javaReferenceType;
		Opacity opacity;
		
		if(term.getName().equals(BB_REF_TERM_FLAG) ||
				(term.arg(1) instanceof Compound && ((Compound)term.arg(1)).getName().equals(BB_REF_TERM_FLAG)) )
			opacity = Opacity.BLACK_BOX;
		else if(term.arg(1) instanceof Compound && ((Compound)term.arg(1)).getName().equals(WB_REF_TERM_FLAG))
			opacity = Opacity.WHITE_BOX;
		else
			throw new JpcException("Unrecognized reference opacity for term: " + term + ".");
		
		if(term.getName().equals(STRONG_REF_FLAG) || term.getName().equals(BB_REF_TERM_FLAG))
			javaReferenceType = ReferenceType.STRONG;
		else if (term.getName().equals(SOFT_REF_FLAG))
			javaReferenceType = ReferenceType.SOFT;
		else if (term.getName().equals(WEAK_REF_FLAG))
			javaReferenceType = ReferenceType.WEAK;
		else
			throw new JpcException("Unrecognized reference type for term: " + term + ".");
		
		return new RefTermType(javaReferenceType, opacity);
	}

}
