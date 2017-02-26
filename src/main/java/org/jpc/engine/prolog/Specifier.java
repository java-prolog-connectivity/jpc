package org.jpc.engine.prolog;

import org.jpc.mapping.converter.TermConvertable;
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

	public boolean isUnary() {
		return toString().length() == 2;
	}
	
	public boolean isBinary() {
		return toString().length() == 3;
	}
	
	public boolean isPrefix() {
		return toString().startsWith("f");
	}
	
	public boolean isPostfix() {
		return toString().endsWith("f");
	}
	
	public boolean isInfix() {
		return isBinary() && toString().substring(1, 2).equals("f");
	}
	
}
