package org.jpc.term.unification;

import java.util.HashMap;
import java.util.Map;

import org.jpc.engine.prolog.PrologConstants;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.JpcPreferences;

import com.google.common.base.Function;

/**
 * Utility class for renaming variables from a term in order to avoid name collisions with other terms that do not share the same lexical scope.
 * @author sergioc
 *
 */
//TODO delete
public class VariableAnonymizerTermExpander implements Function<Term, Term> {

	private final Map<String, Integer> context;
	private int count;
	
	public VariableAnonymizerTermExpander() {
		this(new HashMap<String, Integer>());
	}
	
	public VariableAnonymizerTermExpander(Map<String, Integer> context) {
		this.context = context;
	}
	
	@Override
	public Term apply(Term term) {
		if(term instanceof Var && !((Var)term).getName().equals(PrologConstants.ANONYMOUS_VAR_NAME)) {
			String varName = ((Var)term).getName();
			Integer code = context.get(varName);
			if(code == null) {
				code = count++;
				context.put(varName, code);
			}
			varName = JpcPreferences.JPC_VAR_PREFIX + code;
			return new Var(varName);
		} else
			return null;
	}

}
