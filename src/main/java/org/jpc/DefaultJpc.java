package org.jpc;

import java.lang.reflect.Type;
import java.util.Objects;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.DefaultJpcConverterManager;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.catalog.NullConverter;
import org.jpc.converter.catalog.SystemConverterManager;
import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstantiationManager;
import org.jpc.converter.typesolver.DefaultTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class DefaultJpc extends Jpc {

	private NullConverter nullConverter = new NullConverter();
	private SystemConverterManager systemConverterManager = new SystemConverterManager();
	private ConverterManager converterManager;
	private TypeSolverManager typeSolverManager;
	private InstantiationManager instantiationManager;
	private JTermManager jTermManager;
	private ErrorHandler errorHandler;
	//private JpcPreferences preferences;

	public DefaultJpc() {
		this.converterManager = new DefaultJpcConverterManager();
		this.typeSolverManager = new DefaultTypeSolverManager();
		this.instantiationManager = new DefaultInstantiationManager();
		this.jTermManager = JTermUtil.getJTermManager();
		this.errorHandler = new DefaultJpcErrorHandler();
	}
	
	public DefaultJpc(ConverterManager converterManager, TypeSolverManager typeSolverManager, InstantiationManager instantiationManager, JTermManager jTermManager, ErrorHandler errorHandler) {
		this.typeSolverManager = typeSolverManager;
		this.converterManager = converterManager;
		this.instantiationManager = instantiationManager;
		this.jTermManager = jTermManager;
		this.errorHandler = errorHandler;
		//this.preferences = preferences;
	}
	
	@Override
	public <T> T fromTerm(Term term, Type type) {
		Objects.requireNonNull(term);
		if(!Object.class.equals(type) && TypeWrapper.wrap(type).isAssignableFrom(term.getClass()))
			return (T) term;
		if(term instanceof Var)
			return (T) nullConverter.fromTerm((Var)term, type, this);
		try {
			return (T) systemConverterManager.fromTerm(term, type, this);
		} catch(JpcConversionException e) {}
		
		Type termType = getType(term);
		if(termType != null) {
			try {
				type = TypeWrapper.wrap(termType).mostSpecificType(type); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {} //do nothing
		}
//		try {
			return (T) converterManager.fromTerm(term, type, this);
//		} catch(JpcConversionException e) {
//			if(Object.class.equals(type))
//				return (T) term;
//			else
//				throw e;
//		}
	}
	
	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		if(object==null) {
			if(termClass.isAssignableFrom(Var.class))
				return (T) nullConverter.toTerm(object, this);
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + termClass);
		} 
		if(termClass.isAssignableFrom(object.getClass()))
			return (T) object;
		try {
			return systemConverterManager.toTerm(object, termClass, this);
		} catch(JpcConversionException e) {}
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
	public JTermManager getJTermManager() {
		return jTermManager;
	}

	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

}
