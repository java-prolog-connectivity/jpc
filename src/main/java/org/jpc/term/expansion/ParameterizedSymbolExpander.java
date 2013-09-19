package org.jpc.term.expansion;

import static org.jpc.JpcPreferences.DEFAULT_TERM_CONVERSION_SYMBOL;
import static org.jpc.JpcPreferences.SUBSTITUTION_OPERATOR;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_SERIALIZATION_SYMBOL;

import java.io.Serializable;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.jterm.RefManager;
import org.jpc.term.jterm.Serialized;

import com.google.common.base.Optional;

public class ParameterizedSymbolExpander extends CachedTermExpander {

	private List parameters;
	private Jpc context;
	
	public ParameterizedSymbolExpander(List parameters) {
		this(parameters, JpcBuilder.create().build());
	}
	
	public ParameterizedSymbolExpander(List parameters, Jpc context) {
		this.parameters = parameters;
		this.context = context;
	}
	
	@Override
	protected Optional<Term> doExpand(Term term) {
		Term expanded = null;
		if(term.hasFunctor(SUBSTITUTION_OPERATOR, 1)) {
			Term parameterArgumentTerm = term.arg(1);
			String conversionCode;
			int position;
			if(parameterArgumentTerm instanceof Atom) {
				String name = ((Atom) parameterArgumentTerm).getName();
				verifyOrThrow(name);
				conversionCode = name.substring(0,1);
				position = Integer.parseInt(name.substring(1));
			} else if(parameterArgumentTerm instanceof IntegerTerm) {
				conversionCode = DEFAULT_TERM_CONVERSION_SYMBOL;
				position = ((IntegerTerm) parameterArgumentTerm).intValue();
			} else
				throw new RuntimeException("Wrong operand for " + SUBSTITUTION_OPERATOR);
			Object param = parameters.get(position -1);
			switch(conversionCode) {
				case TERM_CONVERSION_BY_MAPPING_SYMBOL:
					expanded = context.toTerm(param);
					break;
				case TERM_CONVERSION_BY_REFERENCE_SYMBOL:
					expanded = RefManager.jRefTerm(param);
					break;
				case TERM_CONVERSION_BY_SERIALIZATION_SYMBOL:
					expanded = Serialized.jSerializedTerm((Serializable)param);
					break;
			}
		}
		return Optional.fromNullable(expanded);
	}
	
	public static void verifyOrThrow(String symbol) {
		String regex = "(" + TERM_CONVERSION_BY_MAPPING_SYMBOL + "|" + TERM_CONVERSION_BY_REFERENCE_SYMBOL + "|" + TERM_CONVERSION_BY_SERIALIZATION_SYMBOL + ")\\d+";
		if(!symbol.matches(regex))
			throw new RuntimeException("Wrong symbol: " + symbol);
	}
	
}
