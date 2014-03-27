package org.jpc.converter.catalog.jterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.jterm.PrologReferenceType;
import org.jpc.term.jterm.PrologReferenceType.Opacity;
import org.minitoolbox.gc.ReferenceType;

public class TermToPrologReferenceTypeConverter implements FromTermConverter<Term, PrologReferenceType> {

	public static final String REF_FLAG_IDENTIFIER = "ref";
	public static final String JTERM_FLAG_IDENTIFIER = "jterm";
	public static final String STRONG_REF_FLAG_IDENTIFIER = "strong";
	public static final String SOFT_REF_FLAG_IDENTIFIER = "soft";
	public static final String WEAK_REF_FLAG_IDENTIFIER = "weak";
	
	
	@Override
	public PrologReferenceType fromTerm(Term term, Type targetType, Jpc context) {
		ReferenceType javaReferenceType;
		Opacity opacity;
		
		if( (term instanceof Atom && ((Atom)term).getName().equals(REF_FLAG_IDENTIFIER)) 
			|| (term instanceof Compound && ((Compound)term).arg(1).equals(REF_FLAG_IDENTIFIER)) 
			)
			opacity = Opacity.BLACK_BOX;
		else if(term instanceof Compound && ((Compound)term).arg(1).equals(JTERM_FLAG_IDENTIFIER))
			opacity = Opacity.WHITE_BOX;
		else
			throw new JpcException("Unrecognized reference opacity for term: " + term + ".");
		
		if( (term instanceof Atom) || 
				(term instanceof Compound && ((Compound)term).getNameString().equals(STRONG_REF_FLAG_IDENTIFIER)))
			javaReferenceType = ReferenceType.STRONG;
		else if (term instanceof Compound && ((Compound)term).getNameString().equals(SOFT_REF_FLAG_IDENTIFIER))
			javaReferenceType = ReferenceType.SOFT;
		else if (term instanceof Compound && ((Compound)term).getNameString().equals(WEAK_REF_FLAG_IDENTIFIER))
			javaReferenceType = ReferenceType.WEAK;
		else
			throw new JpcException("Unrecognized reference type for term: " + term + ".");
		
		return new PrologReferenceType(javaReferenceType, opacity);
	}

}
