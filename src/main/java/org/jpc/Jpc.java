package org.jpc;

import static java.util.Arrays.asList;
import static org.jconverter.converter.TypeDomain.typeDomain;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.JConverter;
import org.jconverter.converter.TypeDomain;
import org.jpc.mapping.converter.JpcConverterManager;
import org.jconverter.typesolver.TypeSolverManager;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.refterm.RefTermManager;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * @author sergioc
 *
 */
public interface Jpc extends JConverter {

	default Jpc withId(Object ...contextIds) {
		return new JpcImpl(
				getJpcConverterManager(),
				getFactoryManager(),
				asList(contextIds),
				getTypeSolverManager(),
				getRefTermManager(),
				getErrorHandler()
				);
	}

	JpcConverterManager getJpcConverterManager();

	TypeSolverManager getTypeSolverManager();

	ErrorHandler getErrorHandler();

	<T> T fromTerm(Term term);

	default <T> T fromTerm(Term term, Type targetType) {
		return fromTerm(term, typeDomain(targetType));
	}

	<T> T fromTerm(Term term, TypeDomain target);
	
	<T extends Term> T toTerm(Object object);

	default <T extends Term> T toTerm(Object object, Type targetType) {
		return toTerm(object, typeDomain(targetType));
	}

	<T extends Term> T toTerm(Object object, TypeDomain target);
	
	Compound toCompound(Object name, List<?> args);
	
	ListTerm listTerm(Object ...objects);
	
	ListTerm listTerm(List<?> objects);

	/**
	 * 
	 * @param object the object which conversion target type to infer.
	 * @return the inferred type.
	 */
	Type inferType(Object object);

	boolean handleError(Term errorTerm, Term goal);

	RefTermManager getRefTermManager();

	Compound newSoftRefTerm(Object ref, Compound compound);
	
	Compound newSoftRefTerm(Object ref);
	
	Compound newWeakRefTerm(Object ref, Compound compound);
	
	Compound newWeakRefTerm(Object ref);
	
	Compound newRefTerm(Object ref);
	
	Compound newRefTerm(Object ref, Compound compound);
	
	void forgetRefTerm(Compound term);
	
	void forgetRef(Object ref);
	
	Compound refTerm(Object o);
	
	<T> T resolveRefTerm(Compound compound);
	
}
