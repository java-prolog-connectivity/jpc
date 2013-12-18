package org.jpc;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.jconverter.converter.CheckedConverterEvaluator;
import org.jconverter.converter.ConversionException;
import org.jconverter.converter.Converter;
import org.jconverter.instantiation.InstantiationManager;
import org.jconverter.instantiation.JGumInstantiationManager;
import org.jgum.JGum;
import org.jgum.strategy.ChainOfResponsibility;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.FromTermConverterAdapter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.catalog.jterm.FromJTermConverter;
import org.jpc.converter.catalog.jterm.FromSerializedConverter;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;


public class DefaultJpc extends Jpc {
	
	//private final VarConverter nullConverter = new VarConverter();
	private final ChainOfResponsibility<Converter<?,?>,?> fromTermSystemConverter;
	private final JTermManager jTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	
	public DefaultJpc() {
		this(new JGum());
	}
	
	protected DefaultJpc(JGum jgum) {
		this(JpcConverterManager.createDefault(jgum),
				JGumInstantiationManager.createDefault(jgum), 
				JGumTypeSolverManager.createDefault(jgum), 
				JTermUtil.getJTermManager(), 
				new DefaultJpcErrorHandler());
	}
	
	protected DefaultJpc(JpcConverterManager converterManager, InstantiationManager instantiationManager, TypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandler errorHandler) {
		super(converterManager, instantiationManager, typeSolverManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandler = errorHandler;
		fromTermSystemConverter = getFromTermSystemConverter();
	}
	
	private ChainOfResponsibility<Converter<?,?>,?> getFromTermSystemConverter() {
		List<FromTermConverter<?,?>> systemConverters = Arrays.<FromTermConverter<?,?>>asList(new FromJTermConverter(), new FromSerializedConverter());
		return FromTermConverterAdapter.chainConverters((List)systemConverters);
	}
	
	
	@Override
	public <T> T fromTerm(Term term, Type targetType) {
		Objects.requireNonNull(term);
		
		if(!targetType.equals(Object.class) && TypeWrapper.wrap(targetType).isAssignableFrom(term.getClass()))
			return (T) term;
			
		try {
			return (T) fromTermSystemConverter.apply(new CheckedConverterEvaluator(term, targetType, this));
		} catch(ConversionException e) {}
		
		try {
			Type typeSolverType = getType(term);
			if(typeSolverType != null) {
				try {
					targetType = TypeWrapper.wrap(typeSolverType).mostSpecificType(targetType); //will throw an exception if the types are not compatible.
				} catch(IncompatibleTypesException e) {} // the most specific type is not compatible with the target type. Just ignore it and do nothing.
			}
		} catch(UnrecognizedObjectException e){} //a type recommendation cannot be found (do nothing).
		
		return getJpcConverterManager().fromTerm(term, targetType, this);
	}
	
	
	@Override
	public <T extends Term> T toTerm(Object object, Class<T> targetType) {
		if(object==null) {
			if(targetType.isAssignableFrom(Var.class))
				return (T) Var.ANONYMOUS_VAR;
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + targetType);
		}
		
		if(targetType.isAssignableFrom(object.getClass()))
			return (T) object;
		
		try {
			Type typeSolverType = getType(object);
			if(typeSolverType != null) {
				try {
					targetType = (Class) TypeWrapper.wrap(typeSolverType).mostSpecificType(targetType); //will throw an exception if the types are not compatible.
				} catch(IncompatibleTypesException e) {} // the most specific type is not compatible with the target type. Just ignore it and do nothing.
			}
		} catch(UnrecognizedObjectException e){} //a type recommendation cannot be found (do nothing).
		
		return getJpcConverterManager().toTerm(object, targetType, this);
	}
	
	
	private JpcConverterManager getJpcConverterManager() {
		return (JpcConverterManager) converterManager;
	}
	
	@Override
	public JTermManager getJTermManager() {
		return jTermManager;
	}

	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

	@Override
	protected Type getType(Object key, Object object) {
		return typeSolverManager.getType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
}
