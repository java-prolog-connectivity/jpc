package org.jpc.term.jterm;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

import com.google.common.base.Function;

/**
 * An IndexArgumentFunction maps the index of a compound term to one of its arguments in a given position.
 * If the argument is a number of an atom, it will be converted to its equivalent Java type.
 * @see RefMap
 * @author sergioc
 *
 */
public class IndexArgumentFunction implements Function<Compound, Object> {

	private static final IndexArgumentFunction FIRST_ARGUMENT_FUNCTION = new IndexArgumentFunction(1);
	public static IndexArgumentFunction firstArgumentFunction() {return FIRST_ARGUMENT_FUNCTION;}
	
	private final int pos;
	
	/**
	 * 
	 * @param pos the position of the compound argument employed as index.
	 */
	public IndexArgumentFunction(int pos) {
		this.pos = pos;
	}

	@Override
	public Object apply(Compound input) {
		Object key;
		Term keyTerm = input.arg(pos);
		if(keyTerm instanceof NumberTerm)
			key = ((NumberTerm)keyTerm).getValue();
		else if(keyTerm instanceof Atom)
			key = ((Atom)keyTerm).getName();
		else
			key = keyTerm;
		return key;
	}
	
}
