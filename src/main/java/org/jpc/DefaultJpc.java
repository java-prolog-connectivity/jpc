package org.jpc;

import java.lang.reflect.Type;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.TermConvertable;
import org.jpc.converter.catalog.NullConverter;
import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstantiationManager;
import org.jpc.converter.typesolver.DefaultTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JRefManager;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class DefaultJpc extends Jpc {

	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private JRefManager refManager;
	private ErrorHandler errorHandler;
	//private JpcPreferences preferences;

	public DefaultJpc() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.refManager = JRefManager.getJRefManager();
		this.errorHandler = new DefaultJpcErrorHandler();
	}
	
	public DefaultJpc(ConverterManager converterManager, TypeSolverManager typeSolverManager, InstantiationManager instantiationManager, JRefManager refManager, ErrorHandler errorHandler) {
		this.typeSolverManager = typeSolverManager;
		this.converterManager = converterManager;
		this.instantiationManager = instantiationManager;
		this.refManager = refManager;
		this.errorHandler = errorHandler;
		//this.preferences = preferences;
	}
	
	@Override
	public <T> T fromTerm(Term term, Type type) {
		if(term instanceof Var)
			return (T) new NullConverter().fromTerm((Var) term, type, this);
		Type termType = getType(term);
		if(termType != null) {
			try {
				type = TypeWrapper.wrap(termType).mostSpecificType(type); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {} //do nothing
		}
		return (T) converterManager.fromTerm(term, type, this);
	}
	
	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		if(object==null) {
			if(termClass.isAssignableFrom(Var.class))
				return (T) new NullConverter().toTerm(object, Var.class, this);
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + termClass);
		} else if(object instanceof Term)
			return (T) object; // a cast exception if the object is already a term, which is not compatible with the term class sent as argument
		else if(object instanceof TermConvertable)
			return (T) ((TermConvertable)object).asTerm();
		else
			return (T) converterManager.toTerm(object, termClass, this);
	}
	
	@Override
	public <T> T instantiate(Type targetType) {
		return instantiationManager.instantiate(targetType);
	}

	@Override
	public Type getType(Term term) {
		return typeSolverManager.getType(term);
	}
	
	@Override
	public JRefManager getRefManager() {
		return refManager;
	}

	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

}
