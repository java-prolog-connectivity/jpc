package org.jpc;

import static java.util.Arrays.asList;
import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jconverter.converter.TypeDomain.typeDomain;
import static org.jpc.term.Var.dontCare;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Objects;

import org.jconverter.converter.Converter;
import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.InterTypeConverterEvaluator;
import org.jconverter.converter.TypeDomain;
import org.jconverter.factory.FactoryManager;
import org.jconverter.factory.FactoryManagerImpl;
import org.jgum.JGum;
import org.jpc.converter.JpcConverterManager;
import org.jpc.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.converter.catalog.refterm.FromRefTermConverter;
import org.jpc.converter.catalog.refterm.ToRefTermConverter;
import org.jpc.converter.typesolver.JpcTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.internal.jconverter.Adapters;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Number;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.refterm.RefTermManager;
import org.typetools.IncompatibleTypesException;
import org.typetools.typewrapper.TypeWrapper;


public class DefaultJpc extends Jpc {
	
	//private final VarConverter nullConverter = new VarConverter();
	private final Converter<Compound, ?> fromJRefTermConverter;
	private final Converter<?, Compound> toJRefTermConverter;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	private final RefTermManager refTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	
	DefaultJpc() {
		this(new JGum(), new JpcEngine());
	}
	
	DefaultJpc(JGum categorization, JpcEngine embeddedEngine) {
		this(JpcConverterManager.registerDefaults(new JpcConverterManager(categorization, embeddedEngine)),
				FactoryManagerImpl.registerDefaults(new FactoryManagerImpl(categorization)),
				JpcTypeSolverManager.registerDefaults(new JpcTypeSolverManager(categorization, embeddedEngine)),
				new RefTermManager(),
				new DefaultJpcErrorHandler());
	}
	
	/**
	 * 
	 * @param converterManager a converter manager responsible of converting objects.
	 * @param factoryManager a factory manager responsible of instantiating objects.
	 * @param typeSolverManager a type solver manager responsible of recommending types for the result of a conversion.
	 * @param refTermManager an object keeping mappings between terms and Java object references.
	 * @param errorHandler a error handler.
	 */
	DefaultJpc(JpcConverterManager converterManager, FactoryManager factoryManager, TypeSolverManager typeSolverManager, RefTermManager refTermManager, ErrorHandler errorHandler) {
		super(converterManager, factoryManager);
		this.typeSolverManager = typeSolverManager;
		this.refTermManager = refTermManager;
		this.errorHandler = errorHandler;
		fromJRefTermConverter = Adapters.asTypedConverter(new FromRefTermConverter());
		toJRefTermConverter = Adapters.asTypedConverter(new ToRefTermConverter());
	}

	private JpcConverterManager getJpcConverterManager() {
		return (JpcConverterManager) converterManager;
	}
	
	public final <T> T fromTerm(Term term) {
		return fromTerm(term, typeDomain(Object.class));
	}
	
	@Override
	public <T> T fromTerm(Term term, TypeDomain target) {
		Objects.requireNonNull(term);
		Class<?> targetClass = target.getRawClass();
		
		//PERFORMANCE BLOCK (May be deleted. Hardcoding of few primitive conversions just to increase performance).
		if(targetClass.equals(String.class) && term instanceof Atom) { //if the target type is Object better do not take the shortcut below, since many options are possible.
			Atom atom = (Atom)term;
			return (T) atom.getName();
		}
		if((targetClass.equals(Object.class) || java.lang.Number.class.isAssignableFrom(targetClass)) && term instanceof Number) {
			return (T) new NumberToNumberTermConverter()
					.fromTerm((Number) term, targetClass.equals(Object.class) ? typeDomain(java.lang.Number.class) : target, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if(!targetClass.equals(Object.class) && typeDomain(term.getClass()).isSubsetOf(target)) {
			return (T) term;
		}
		
		if(term instanceof Compound) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T) new InterTypeConverterEvaluator(conversionGoal(term, target), this).apply(fromJRefTermConverter);
			} catch (DelegateConversionException e) {}
		}
		
		try {
			Type typeSolverType = inferType(term);
			if(typeSolverType != null) {
				try {
					Type moreSpecificType = TypeWrapper.wrap(typeSolverType).mostSpecificType(target.getType()); //will throw an exception if the types are not compatible.
					target = typeDomain(moreSpecificType);
				} catch(IncompatibleTypesException e) {} // the most specific type is not compatible with the target type. Just ignore it and do nothing.
			}
		} catch(UnrecognizedObjectException e){} //a type recommendation cannot be found (do nothing).
		
		return getJpcConverterManager().fromTerm(term, target, this);
	}
	
	public final <T extends Term> T toTerm(Object object) {
		return (T) toTerm(object, typeDomain(Term.class));
	}



	@Override
	public <T extends Term> T toTerm(Object object, TypeDomain target) {
		Class<?> targetClass = target.getRawClass();
		if (object==null) {
			if(targetClass.isAssignableFrom(Var.class))
				return (T) dontCare();
			else
				throw new NullPointerException("A Null object cannot be transformed to target logic term: " + target);
		}
		
		
		//PERFORMANCE BLOCK (May be deleted. Hardcoding of few primitive conversions just to increase performance).
		if (targetClass.isAssignableFrom(Atom.class) && object instanceof String) {
			return (T) new Atom((String)object);
		}	
		if ((targetClass.equals(Term.class) || Number.class.isAssignableFrom(targetClass)) && object instanceof java.lang.Number) {
			return (T) new NumberToNumberTermConverter()
					.toTerm((java.lang.Number)object, targetClass.equals(Term.class) ? Number.class : targetClass, this);
		}
		//--- END OF PERFORMANCE BLOCK
		
		
		if (targetClass.isAssignableFrom(object.getClass())) //the object is already an instance of the desired term.
			return (T) object;
		
		if (!(object instanceof String || object instanceof java.lang.Number || object instanceof Boolean || object instanceof Character)) { //condition added to increase performance, the check is not needed otherwise.
			try {
				return (T) new InterTypeConverterEvaluator(conversionGoal(object, target), this).apply(toJRefTermConverter);
			} catch(DelegateConversionException e) {}
		}
		
		try {
			Type typeSolverType = inferType(object);
			if(typeSolverType != null) {
				try {
					targetClass = (Class) TypeWrapper.wrap(typeSolverType).mostSpecificType(targetClass); //will throw an exception if the types are not compatible.
					target = typeDomain(targetClass);
				} catch(IncompatibleTypesException e) {} // the most specific type is not compatible with the target type. Just ignore it and do nothing.
			}
		} catch(UnrecognizedObjectException e){} //a type recommendation cannot be found (do nothing).
		
		return getJpcConverterManager().toTerm(object, target, this);
	}
	
	@Override
	public final Compound toCompound(Object name, List<?> args) {
		return new Compound((Term)toTerm(name), listTerm(args));
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
	public Type inferType(Object object) {
		return inferType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
	/**
	 * 
	 * @param key constrains the type solvers that will be looked up in this operation.
	 * @param object the object which conversion target type to recommend.
	 * @return the recommended type.
	 */
	private Type inferType(Object key, Object object) {
		return typeSolverManager.inferType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
	@Override
	public RefTermManager getRefTermManager() {
		return refTermManager;
	}
	
	@Override
	public Compound newSoftRefTerm(Object ref, Compound compound) {
		return refTermManager.newSoftRefTerm(ref, compound);
	}
	
	@Override
	public Compound newSoftRefTerm(Object ref) {
		return refTermManager.newSoftRefTerm(ref);
	}
	
	@Override
	public Compound newWeakRefTerm(Object ref, Compound compound) {
		return refTermManager.newWeakRefTerm(ref, compound);
	}
	
	@Override
	public Compound newWeakRefTerm(Object ref) {
		return refTermManager.newWeakRefTerm(ref);
	}
	
	@Override
	public Compound newRefTerm(Object ref) {
		return refTermManager.newRefTerm(ref);
	}
	
	@Override
	public Compound newRefTerm(Object ref, Compound compound) {
		return refTermManager.newRefTerm(ref, compound);
	}
	
	@Override
	public void forgetRefTerm(Compound term) {
		refTermManager.forgetRefTerm(term);
	}
	
	@Override
	public void forgetRef(Object ref) {
		refTermManager.forgetRef(ref);
	}
	
	@Override
	public Compound refTerm(Object o) {
		return refTermManager.refTerm(o);
	}
	
	@Override
	public <T> T resolveRefTerm(Compound compound) {
		return refTermManager.resolve(compound);
	}
	
}
