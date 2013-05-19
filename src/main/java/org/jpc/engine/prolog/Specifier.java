package org.jpc.engine.prolog;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;

/**
 * This class is experimental and may be deleted.
 * @author sergioc
 *
 */
public enum Specifier implements TermConvertable<Atom> {
	FX, FY, XF, YF, XFX, YFX, XFY;
	
	@Override
	public String toString() {
		return super.toString().toLowerCase();
	}
	
	@Override
	public Atom asTerm() {
		return new Atom(toString());
	}

	public static void main(String[] args) {
		System.out.println(FX.asTerm());
	}
}
