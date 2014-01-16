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
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.ToTermConverterAdapter;
import org.jpc.converter.catalog.jterm.FromJTermConverter;
import org.jpc.converter.catalog.jterm.ToJTermConverter;
import org.jpc.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.converter.catalog.serialized.FromSerializedConverter;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

//TODO merge with JPC ?
public class DefaultJpc extends Jpc {
	
	//private final VarConverter nullConverter = new VarConverter();
	private final ChainOfResponsibility<Converter<?,?>,?> fromTermSystemConverter;
	private final ChainOfResponsibility<Converter<?,?>,?> toTermSystemConverter;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	private final JTermManager jTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	
	public DefaultJpc() {
		this(new JGum());
	}
	
	protected DefaultJpc(JGum jgum) {
		this(JpcConverterManager.createDefault(jgum),
				JGumInstantiationManager.createDefault(jgum), 
				JGumTypeSolverManager.createDefault(jgum), 
				new JTermManager(),
				new DefaultJpcErrorHandler());
	}
	
	protected DefaultJpc(JpcConverterManager converterManager, InstantiationManager instantiationManager, TypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandler errorHandler) {
		super(converterManager, instantiationManager, typeSolverManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandler = errorHandler;
		fromTermSystemConverter = getFromTermSystemConverter();
		toTermSystemConverter = getToTermSystemConverter();
	}
	
	private ChainOfResponsibility<Converter<?,?>,?> getFromTermSystemConverter() {
		List<FromTermConverter<?,?>> systemConverters = Arrays.<FromTermConverter<?,?>>asList(new FromJTermConverter(), new FromSerializedConverter());
		return FromTermConverterAdapter.chainConverters((List)systemConverters);
	}
	
	private ChainOfResponsibility<Converter<?,?>,?> getToTermSystemConverter() {
		List<ToTermConverter<?,?>> systemConverters = Arrays.<ToTermConverter<?,?>>asList(new ToJTermConverter());
		return ToTermConverterAdapter.chainConverters((List)systemConverters);
	}
	
	@Override
	public <T> T fromTerm(Term term, Type targetType) {
		Objects.requireNonNull(term);
		
		TypeWrapper wrappedTargetType = TypeWrapper.wrap(targetType);
		
		
		//PERFORMANCE BLOCK (May be deleted. Hardcoding of few primitive conversions just to increase performance).
		if(targetType.equals(String.class) && term instanceof Atom) { //if the target type is Object better do not take the shortcut below, since many options are possible.
			Atom atom = (Atom)term;
			return (T) atom.getName();
		}
		if((wrappedTargetType.equals(Object.class) || Number.class.isAssignableFrom(wrappedTargetType.getRawClass())) && term instanceof NumberTerm) {
			return (T) new NumberToNumberTermConverter().fromTerm(term, targetType.equals(Object.class)?Number.class:targetType, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if(!targetType.equals(Object.class) && wrappedTargetType.isAssignableFrom(term.getClass()))
			return (T) term;
		
		if(term instanceof Compound) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T) fromTermSystemConverter.apply(new CheckedConverterEvaluator(term, targetType, this));
			} catch(ConversionException e) {}
		}
		
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
		
		
		//PERFORMANCE BLOCK (May be deleted. Hardcoding of few primitive conversions just to increase performance).
		if(targetType.isAssignableFrom(Atom.class) && object instanceof String) {
			return (T)new Atom((String)object);
		}	
		if((targetType.equals(Term.class) || NumberTerm.class.isAssignableFrom(targetType)) && object instanceof Number) {
			return (T) new NumberToNumberTermConverter().toTerm(object, targetType.equals(Term.class)?NumberTerm.class:targetType, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if(targetType.isAssignableFrom(object.getClass())) //the object is already an instance of the desired term.
			return (T) object;
		
		if(!(object instanceof String || object instanceof Number || object instanceof Boolean || object instanceof Character)) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T) toTermSystemConverter.apply(new CheckedConverterEvaluator(object, targetType, this));
			} catch(ConversionException e) {}
		}
		
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
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

	@Override
	protected Type getType(Object key, Object object) {
		return typeSolverManager.getType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
	@Override
	public JTermManager getJTermManager() {
		return jTermManager;
	}
	
	@Override
	public Compound newWeakJTerm(Object ref, Compound compound) {
		return jTermManager.newWeakJTerm(ref, compound);
	}
	
	@Override
	public Compound newWeakJTerm(Object ref) {
		return jTermManager.newWeakJTerm(ref);
	}
	
	@Override
	public Compound newJTerm(Object ref) {
		return jTermManager.newJTerm(ref);
	}
	
	@Override
	public Compound newJTerm(Object ref, Compound compound) {
		return jTermManager.newJTerm(ref, compound);
	}
	
	@Override
	public void forgetJTerm(Compound term) {
		jTermManager.forgetJTerm(term);
	}
	
	@Override
	public void forgetJTermRef(Object ref) {
		jTermManager.forgetJTermRef(ref);
	}
	
	@Override
	public Compound jTerm(Object o) {
		return jTermManager.jTerm(o);
	}
	
	@Override
	public Object resolveJTerm(Compound compound) {
		return jTermManager.resolve(compound);
	}
	
}
