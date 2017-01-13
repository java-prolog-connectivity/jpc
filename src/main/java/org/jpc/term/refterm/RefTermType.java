package org.jpc.term.refterm;

import static org.jpc.internal.gc.ReferenceType.SOFT;
import static org.jpc.internal.gc.ReferenceType.STRONG;
import static org.jpc.internal.gc.ReferenceType.WEAK;
import static org.jpc.term.refterm.RefTermType.Opacity.BLACK_BOX;
import static org.jpc.term.refterm.RefTermType.Opacity.WHITE_BOX;

import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.internal.gc.ReferenceType;
import org.jpc.term.Compound;
import org.jpc.term.Term;


public class RefTermType {

	public static enum Opacity{WHITE_BOX, BLACK_BOX}
	
	private final ReferenceType javaReferenceType;
	private final Opacity opacity;
	
	public RefTermType(ReferenceType javaReferenceType, Opacity opacity) {
		this.javaReferenceType = javaReferenceType;
		this.opacity = opacity;
	}
	
	public Term toTerm(Object ref, Jpc jpc) {
		if(javaReferenceType == null) {
			return jpc.toTerm(ref);
		} else if(opacity.equals(BLACK_BOX)) {
			if(javaReferenceType.equals(WEAK)) {
				return jpc.newWeakRefTerm(ref);
			} else if(javaReferenceType.equals(SOFT)) {
				return jpc.newSoftRefTerm(ref);
			} else if(javaReferenceType.equals(STRONG))
				return jpc.newRefTerm(ref);
			else
				throw new JpcException("Unrecognized reference type: " + javaReferenceType);
		} else if(opacity.equals(WHITE_BOX)) {
			if(javaReferenceType.equals(WEAK)) {
				return jpc.newWeakRefTerm(ref, (Compound)jpc.toTerm(ref));
			} else if(javaReferenceType.equals(SOFT)) {
				return jpc.newSoftRefTerm(ref, (Compound)jpc.toTerm(ref));
			} else if(javaReferenceType.equals(STRONG))
				return jpc.newRefTerm(ref, (Compound)jpc.toTerm(ref));
			else
				throw new JpcException("Unrecognized reference type: " + javaReferenceType);
		} else
			throw new JpcException("Unrecognized reference opacity: " + opacity);
	}
	
}
