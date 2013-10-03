package org.jpc.term.expansion;

import static org.jpc.JpcPreferences.DEFAULT_TERM_CONVERSION_SYMBOL;
import static org.jpc.JpcPreferences.SUBSTITUTION_OPERATOR;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_AND_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_SERIALIZATION_SYMBOL;

import java.io.Serializable;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.jterm.JRefManager;
import org.jpc.term.jterm.Serialized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class ParameterizedSymbolExpander extends CachedTermExpander {

	private static Logger logger = LoggerFactory.getLogger(ParameterizedSymbolExpander.class);

	private Jpc context;
	
	public ParameterizedSymbolExpander() {
		this(JpcBuilder.create().build());
	}
	
	public ParameterizedSymbolExpander(Jpc context) {
		this.context = context;
	}
	
	@Override
	protected Term doExpand(Term term) {
		Term expanded = null;
		if(term instanceof Compound) {
			Compound compound = (Compound) term;
			int arity = compound.arity();
			if(compound.hasName(SUBSTITUTION_OPERATOR) && (arity == 1 || arity == 2)) {
				String conversionCode;
				Term symbolTerm;
				if(arity == 1) {
					conversionCode = DEFAULT_TERM_CONVERSION_SYMBOL;
					symbolTerm = term.arg(1);
				} else { //arity ==2
					Term conversionCodeTerm = term.arg(1);
					if(!(conversionCodeTerm instanceof Atom)) {//this is not necessarily an error, since the symbol '/' may be used with another meaning in the program.
						return null;
					}
					conversionCode = ((Atom)conversionCodeTerm).getName();
					verifyOrThrow(conversionCode);
					symbolTerm = term.arg(2);
				}
				Object resolved = resolve(symbolTerm);
				switch(conversionCode) {
					case TERM_CONVERSION_BY_MAPPING_SYMBOL:
						expanded = context.toTerm(resolved);
						break;
					case TERM_CONVERSION_BY_REFERENCE_SYMBOL:
						expanded = JRefManager.jRefTerm(resolved);
						break;
					case TERM_CONVERSION_BY_SERIALIZATION_SYMBOL:
						expanded = Serialized.jSerializedTerm((Serializable)resolved);
						break;
				}
			}
		}
		return expanded;
	}
	
	public abstract Object resolve(Term symbolTerm);
	
	public static boolean isValidConversion(String conversionCode) {
		String regex = "(" + TERM_CONVERSION_BY_MAPPING_SYMBOL + "|" + TERM_CONVERSION_BY_REFERENCE_SYMBOL + "|" + TERM_CONVERSION_BY_SERIALIZATION_SYMBOL + "|" + TERM_CONVERSION_BY_MAPPING_AND_REFERENCE_SYMBOL + ")";
		return conversionCode.matches(regex);
	}
	
	public static void verifyOrThrow(String conversionCode) {
		if(!isValidConversion(conversionCode))
			throw new RuntimeException("Not a valid conversion code: " + conversionCode);
	}
	
}
