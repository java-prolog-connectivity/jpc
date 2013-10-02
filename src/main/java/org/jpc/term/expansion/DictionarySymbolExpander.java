package org.jpc.term.expansion;

import java.util.Map;

import org.jpc.Jpc;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class DictionarySymbolExpander extends ParameterizedSymbolExpander {

	private Map<String,?> map;
	
	public DictionarySymbolExpander(Map<String,?> map) {
		super();
		this.map = map;
	}
	
	public DictionarySymbolExpander(Map<String,?> map, Jpc context) {
		super(context);
		this.map = map;
	}
	
	public Object resolve(Term symbolTerm) {
		if(!(symbolTerm instanceof Atom)) {
			throw new RuntimeException("Wrong replacement symbol: " + symbolTerm);
		}
		Atom atomSymbolTerm = (Atom) symbolTerm;
		return map.get(atomSymbolTerm.getName());
	}
	
}
