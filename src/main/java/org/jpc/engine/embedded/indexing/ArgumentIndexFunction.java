package org.jpc.engine.embedded.indexing;

import org.jpc.term.Atom;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

import com.google.common.base.Function;

/**
 * An ArgumentIndexFunction maps the index of a compound term to one of its arguments in a given position.
 * If the argument is a number or an atom, it will be converted to its equivalent Java type.
 * @see CompoundIndex
 * @author sergioc
 *
 */
public class ArgumentIndexFunction implements Function<Term, Object> {

	private static final ArgumentIndexFunction FIRST_ARGUMENT_FUNCTION = new ArgumentIndexFunction(1);
	public static ArgumentIndexFunction firstArgumentFunction() {return FIRST_ARGUMENT_FUNCTION;}
	
	private final int pos;
	
	/**
	 * 
	 * @param pos the position of the compound argument employed as index.
	 */
	public ArgumentIndexFunction(int pos) {
		this.pos = pos;
	}

	@Override
	public Object apply(Term term) {
		Object key;
		Term keyTerm;
		try {
			keyTerm = term.arg(pos);
		} catch(IndexOutOfBoundsException e) {
			throw new NonIndexableTermException(term, this);
		}
		if(!keyTerm.isGround())
			throw new NonIndexableTermException(term, this);
		if(keyTerm instanceof NumberTerm)
			key = ((NumberTerm)keyTerm).getValue();
		else if(keyTerm instanceof Atom)
			key = ((Atom)keyTerm).getName();
		else
			key = keyTerm;
		return key;
	}
	
}
