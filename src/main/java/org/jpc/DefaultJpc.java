package org.jpc;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Objects;

import org.jconverter.converter.ConversionException;
import org.jconverter.converter.Converter;
import org.jconverter.converter.ConverterEvaluator;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.JGumFactoryManager;
import org.jgum.JGum;
import org.jpc.converter.FromTermConverterAdapter;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.ToTermConverterAdapter;
import org.jpc.converter.catalog.jterm.FromJTermConverter;
import org.jpc.converter.catalog.jterm.ToJTermConverter;
import org.jpc.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.converter.typesolver.JpcTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class DefaultJpc extends Jpc {
	
	//private final VarConverter nullConverter = new VarConverter();
	private final Converter<Compound, ?> fromJTermConverter;
	private final Converter<?, Compound> toJTermConverter;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	private final JTermManager jTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	
	public DefaultJpc() {
		this(new JGum(), new JpcEngine());
	}
	
	private DefaultJpc(JGum jgum, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(jgum, embeddedEngine)),
				JGumFactoryManager.registerDefaults(new JGumFactoryManager(jgum)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(jgum, embeddedEngine)),
				new JTermManager(),
				new DefaultJpcErrorHandler());
	}
	
	/**
	 * 
	 * @param converterManager a converter manager responsible of converting objects.
	 * @param factoryManager a factory manager responsible of instantiating objects.
	 * @param typeSolverManager a type solver manager responsible of recommending types for the result of a conversion.
	 * @param jTermManager an object keeping mappings between terms and Java object references.
	 * @param errorHandler a error handler.
	 */
	public DefaultJpc(JpcConverterManager converterManager, FactoryManager factoryManager, TypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandler errorHandler) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandler = errorHandler;
		fromJTermConverter = FromTermConverterAdapter.forConverter(new FromJTermConverter());
		toJTermConverter = ToTermConverterAdapter.forConverter(new ToJTermConverter());
	}

	private JpcConverterManager getJpcConverterManager() {
		return (JpcConverterManager) converterManager;
	}
	
	public final <T> T fromTerm(Term term) {
		return fromTerm(term, Object.class);
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
			return (T) new NumberToNumberTermConverter().fromTerm((NumberTerm)term, targetType.equals(Object.class)?Number.class:targetType, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if(!targetType.equals(Object.class) && wrappedTargetType.isAssignableFrom(term.getClass()))
			return (T) term;
		
		if(term instanceof Compound) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T)new ConverterEvaluator(term, targetType, this).apply(fromJTermConverter);
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
	
	public final <T extends Term> T toTerm(Object object) {
		return (T) toTerm(object, Term.class);
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
			return (T) new NumberToNumberTermConverter().toTerm((Number)object, targetType.equals(Term.class)?NumberTerm.class:targetType, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if(targetType.isAssignableFrom(object.getClass())) //the object is already an instance of the desired term.
			return (T) object;
		
		if(!(object instanceof String || object instanceof Number || object instanceof Boolean || object instanceof Character)) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T) new ConverterEvaluator(object, targetType, this).apply(toJTermConverter);
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
	
	@Override
	public final Compound toCompound(Object name, List<?> args) {
		return new Compound(toTerm(name), listTerm(args));
	}
	
	@Override
	public final ListTerm listTerm(Object ...objects) {
		return listTerm(asList(objects));
	}
	
	@Override
	public final ListTerm listTerm(List<?> objects) {
		ListTerm listTerm = new ListTerm();
		for(Object o : objects) {
			listTerm.add(toTerm(o));
		}
		return listTerm;
	}
	


	@Override
	public boolean handleError(Term errorTerm, Term goal) {
		return errorHandler.handle(errorTerm, goal, this);
	}

	@Override
	public Type getType(Object object) {
		return getType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
	/**
	 * 
	 * @param key constrains the type solvers that will be looked up in this operation.
	 * @param object the object which conversion target type to recommend.
	 * @return the recommended type.
	 */
	private Type getType(Object key, Object object) {
		return typeSolverManager.getType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
	@Override
	public JTermManager getJTermManager() {
		return jTermManager;
	}
	
	@Override
	public Compound newSoftJTerm(Object ref, Compound compound) {
		return jTermManager.newSoftJTerm(ref, compound);
	}
	
	@Override
	public Compound newSoftJTerm(Object ref) {
		return jTermManager.newSoftJTerm(ref);
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
	public <T> T resolveJTerm(Compound compound) {
		return jTermManager.resolve(compound);
	}
	
}
