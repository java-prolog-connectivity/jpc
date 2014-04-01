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
import org.jpc.converter.catalog.jrefterm.FromJRefTermConverter;
import org.jpc.converter.catalog.jrefterm.ToJRefTermConverter;
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
import org.jpc.term.jrefterm.JRefTermManager;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class DefaultJpc extends Jpc {
	
	//private final VarConverter nullConverter = new VarConverter();
	private final Converter<Compound, ?> fromJRefTermConverter;
	private final Converter<?, Compound> toJRefTermConverter;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	private final JRefTermManager jRefTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	
	DefaultJpc() {
		this(new JGum(), new JpcEngine());
	}
	
	DefaultJpc(JGum jgum, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(jgum, embeddedEngine)),
				JGumFactoryManager.registerDefaults(new JGumFactoryManager(jgum)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(jgum, embeddedEngine)),
				new JRefTermManager(),
				new DefaultJpcErrorHandler());
	}
	
	/**
	 * 
	 * @param converterManager a converter manager responsible of converting objects.
	 * @param factoryManager a factory manager responsible of instantiating objects.
	 * @param typeSolverManager a type solver manager responsible of recommending types for the result of a conversion.
	 * @param jRefTermManager an object keeping mappings between terms and Java object references.
	 * @param errorHandler a error handler.
	 */
	DefaultJpc(JpcConverterManager converterManager, FactoryManager factoryManager, TypeSolverManager typeSolverManager, JRefTermManager jRefTermManager, ErrorHandler errorHandler) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.jRefTermManager = jRefTermManager;
		this.errorHandler = errorHandler;
		fromJRefTermConverter = FromTermConverterAdapter.forConverter(new FromJRefTermConverter());
		toJRefTermConverter = ToTermConverterAdapter.forConverter(new ToJRefTermConverter());
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
				return (T)new ConverterEvaluator(term, targetType, this).apply(fromJRefTermConverter);
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
				return (T) new ConverterEvaluator(object, targetType, this).apply(toJRefTermConverter);
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
	public JRefTermManager getJRefTermManager() {
		return jRefTermManager;
	}
	
	@Override
	public Compound newSoftJRefTerm(Object ref, Compound compound) {
		return jRefTermManager.newSoftJRefTerm(ref, compound);
	}
	
	@Override
	public Compound newSoftJRefTerm(Object ref) {
		return jRefTermManager.newSoftJRefTerm(ref);
	}
	
	@Override
	public Compound newWeakJRefTerm(Object ref, Compound compound) {
		return jRefTermManager.newWeakJRefTerm(ref, compound);
	}
	
	@Override
	public Compound newWeakJRefTerm(Object ref) {
		return jRefTermManager.newWeakJRefTerm(ref);
	}
	
	@Override
	public Compound newJRefTerm(Object ref) {
		return jRefTermManager.newJRefTerm(ref);
	}
	
	@Override
	public Compound newJRefTerm(Object ref, Compound compound) {
		return jRefTermManager.newJRefTerm(ref, compound);
	}
	
	@Override
	public void forgetJRefTerm(Compound term) {
		jRefTermManager.forgetJRefTerm(term);
	}
	
	@Override
	public void forgetJRefTermRef(Object ref) {
		jRefTermManager.forgetRef(ref);
	}
	
	@Override
	public Compound jRefTerm(Object o) {
		return jRefTermManager.jRefTerm(o);
	}
	
	@Override
	public <T> T resolveJRefTerm(Compound compound) {
		return jRefTermManager.resolve(compound);
	}
	
}
