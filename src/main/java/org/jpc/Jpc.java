package org.jpc;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstantiationManager;
import org.jpc.converter.typesolver.DefaultTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

/**
 * A class providing an interface for the main JPC functionality (such as converting between terms and Java objects)
 * This class is inspired by the Gson class from the Gson library (http://code.google.com/p/google-gson/)
 * @author sergioc
 *
 */
public class Jpc {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private ErrorHandler errorHandler;
	//private JpcPreferences preferences;
	
	private static final Version version = new Version();
	public static Version version() {
		return version;
	}
	
	public Jpc() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.errorHandler = new DefaultJpcErrorHandler();
	}
	
	public Jpc(ConverterManager converterManager, TypeSolverManager typeSolverManager, InstantiationManager instantiationManager, ErrorHandler errorHandler) {
		this.typeSolverManager = typeSolverManager;
		this.converterManager = converterManager;
		this.instantiationManager = instantiationManager;
		this.errorHandler = errorHandler;
		//this.preferences = preferences;
	}
	
	public <T> T fromTerm(Term term) {
		return fromTerm(term, Object.class);
	}
	
	public <T> T fromTerm(Term term, Type type) {
		return (T) converterManager.fromTerm(term, type, this);
	}
	
	public Term toTerm(Object object) {
		return toTerm(object, Term.class);
	}
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		return converterManager.toTerm(object, termClass, this);
	}

	public Compound toTerm(Object name, List<? extends Object> args) {
		return new Compound(toTerm(name), listTerm(args));
	}
	
	public ListTerm listTerm(Object ...objects) {
		return listTerm(Arrays.asList(objects));
	}
	
	public ListTerm listTerm(List<? extends Object> objects) {
		ListTerm listTerm = new ListTerm();
		for(Object o : objects) {
			listTerm.add(toTerm(o));
		}
		return listTerm;
	}
	
	public <T> T instantiate(Type targetType) {
		return instantiationManager.instantiate(targetType);
	}

	public Type getType(Term term) {
		return typeSolverManager.getType(term);
	}

	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

}
