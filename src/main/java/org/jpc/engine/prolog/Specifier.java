package org.jpc.engine.prolog;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;

/**
 * This class is experimental and may be deleted.
 * @author sergioc
 *
 */
public enum Specifier implements TermConvertable<Atom> {
	fx, fy, xf, yf, xfx, yfx, xfy;
	
	@Override
	public String toString() {
		return super.toString();
	}
	
	@Override
	public Atom asTerm() {
		return new Atom(toString());
	}

}
