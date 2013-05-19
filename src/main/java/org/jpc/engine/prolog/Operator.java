package org.jpc.engine.prolog;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;

/**
 * This class is experimental and may be deleted.
 * @author sergioc
 *
 */
public class Operator implements TermConvertable<Atom> {

	private String name;
	private Specifier specifier;
	private String priority;
	
	public Operator(String name, Specifier specifier, String priority) {
		this.name = name;
		this.specifier = specifier;
		this.priority = priority;
	}

	public String getName() {
		return name;
	}
	
	public Specifier getSpecifier() {
		return specifier;
	}
	
	public String getPriority() {
		return priority;
	}

	@Override
	public Atom asTerm() {
		return new Atom(name);
	}

}
