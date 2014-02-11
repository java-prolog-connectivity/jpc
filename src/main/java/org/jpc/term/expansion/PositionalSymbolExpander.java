package org.jpc.term.expansion;

import java.util.List;

import org.jpc.Jpc;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;

public class PositionalSymbolExpander extends ParameterizedSymbolExpander {

	private List<?> parameters;
	
	public PositionalSymbolExpander(List<?> parameters) {
		this.parameters = parameters;
	}
	
	public PositionalSymbolExpander(List<?> parameters, Jpc context) {
		super(context);
		this.parameters = parameters;
	}
	
	public Object resolve(Term symbolTerm) {
		if(!(symbolTerm instanceof IntegerTerm)) {
			throw new RuntimeException("Wrong replacement symbol: " + symbolTerm);
		}
		IntegerTerm positionTerm = (IntegerTerm) symbolTerm;
		return parameters.get(positionTerm.intValue()-1);
	}

}
