package org.jpc.term.jterm;

import static org.jpc.term.jterm.PrologReferenceType.Opacity.BLACK_BOX;
import static org.jpc.term.jterm.PrologReferenceType.Opacity.WHITE_BOX;
import static org.minitoolbox.gc.ReferenceType.SOFT;
import static org.minitoolbox.gc.ReferenceType.STRONG;
import static org.minitoolbox.gc.ReferenceType.WEAK;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.gc.ReferenceType;

public class PrologReferenceType {

	public static enum Opacity{WHITE_BOX, BLACK_BOX}
	
	private final ReferenceType javaReferenceType;
	private final Opacity opacity;
	
	public PrologReferenceType(ReferenceType javaReferenceType, Opacity opacity) {
		this.javaReferenceType = javaReferenceType;
		this.opacity = opacity;
	}
	
	public Term toTerm(Object ref, Jpc jpc) {
		if(javaReferenceType == null) {
			return jpc.toTerm(ref);
		} else if(opacity.equals(BLACK_BOX)) {
			if(javaReferenceType.equals(WEAK)) {
				return jpc.newWeakJTerm(ref);
			} else if(javaReferenceType.equals(SOFT)) {
				return jpc.newSoftJTerm(ref);
			} else if(javaReferenceType.equals(STRONG))
				return jpc.newJTerm(ref);
			else
				throw new JpcException("Unrecognized reference type: " + javaReferenceType);
		} else if(opacity.equals(WHITE_BOX)) {
			if(javaReferenceType.equals(WEAK)) {
				return jpc.newWeakJTerm(ref, (Compound)jpc.toTerm(ref));
			} else if(javaReferenceType.equals(SOFT)) {
				return jpc.newSoftJTerm(ref, (Compound)jpc.toTerm(ref));
			} else if(javaReferenceType.equals(STRONG))
				return jpc.newJTerm(ref, (Compound)jpc.toTerm(ref));
			else
				throw new JpcException("Unrecognized reference type: " + javaReferenceType);
		} else
			throw new JpcException("Unrecognized reference opacity: " + opacity);
	}
	
}
