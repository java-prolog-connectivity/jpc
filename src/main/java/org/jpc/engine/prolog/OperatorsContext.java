package org.jpc.engine.prolog;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.CURRENT_OP;
import static org.jpc.engine.prolog.PrologConstants.FINDALL;
import static org.jpc.util.JpcPreferences.JPC_ANON_VAR_PREFIX;

import java.util.Collection;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Multimap;

public class OperatorsContext {

	private static final OperatorsContext EMPTY = new OperatorsContext(
			ImmutableMultimap.<String, Operator>builder().build());

	public static String ALL_OPERATORS_VAR_NAME = JPC_ANON_VAR_PREFIX + "ALL_OPERATORS";
	public static String OP_PRIORITY_VAR_NAME = JPC_ANON_VAR_PREFIX + "OP_PRIORITY";
	public static String OP_SPECIFIER_VAR_NAME = JPC_ANON_VAR_PREFIX + "OP_SPECIFIER";
	public static String OP_NAME_VAR_NAME = JPC_ANON_VAR_PREFIX + "OP_NAME";


	public static Term findAllOperatorsTerm() {
		return new Compound(FINDALL, asList(
				ListTerm.create(new Var(OP_PRIORITY_VAR_NAME), new Var(OP_SPECIFIER_VAR_NAME), new Var(OP_NAME_VAR_NAME)).asTerm(),
				new Compound(CURRENT_OP, asList(new Var(OP_PRIORITY_VAR_NAME), new Var(OP_SPECIFIER_VAR_NAME), new Var(OP_NAME_VAR_NAME))),
				new Var(ALL_OPERATORS_VAR_NAME)));
	}

	private Multimap<String, Operator> operatorsMultimap;

	public OperatorsContext(Multimap<String, Operator> operatorsMultimap) {
		this.operatorsMultimap = operatorsMultimap;
	}
	
	public static OperatorsContext empty() {
		return EMPTY;
	}

	public static OperatorsContext asOperatorsContext(Iterable<? extends Term> operatorsListTerm) {
		Multimap<String, Operator> operatorsMultimap = ArrayListMultimap.<String, Operator>create();
		for(Term operatorTerm : operatorsListTerm) {
			Operator op = Operator.asOperator(operatorTerm);
			operatorsMultimap.put(op.getName(), op);
		}
		return new OperatorsContext(operatorsMultimap);
	}

	public Collection<Operator> getOperators(String name) {
		return operatorsMultimap.get(name);
	}

	public Operator getOperator(Term term) {
		if(term instanceof Compound && term.getArity() <= 2) { //the term is a compound with one or two arguments
			Compound compound = (Compound) term;
			Term compoundName = compound.getNameTerm();
			if(compoundName instanceof Atom) {
				Collection<Operator> operators = getOperators(((Atom) compoundName).getName());
				for(Operator operator : operators) {
					if(compound.usesOperator(operator))
						return operator;
				}
			}
		}
		return null;
	}

}
