package org.jpc;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.jconverter.converter.CheckedConverterEvaluator;
import org.jconverter.converter.ConversionException;
import org.jconverter.converter.Converter;
import org.jconverter.converter.ConverterManager;
import org.jconverter.converter.JGumConverterManager;
import org.jconverter.instantiation.InstantiationManager;
import org.jconverter.instantiation.JGumInstantiationManager;
import org.jgum.JGum;
import org.jgum.strategy.ChainOfResponsibility;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.FromTermConverterAdapter;
import org.jpc.converter.catalog.datetime.CalendarConverter;
import org.jpc.converter.catalog.datetime.XmlGregorianCalendarConverter;
import org.jpc.converter.catalog.error.DomainErrorConverter;
import org.jpc.converter.catalog.error.EvaluationErrorConverter;
import org.jpc.converter.catalog.error.ExistenceErrorConverter;
import org.jpc.converter.catalog.error.InstantiationErrorConverter;
import org.jpc.converter.catalog.error.IsoPrologErrorConverter;
import org.jpc.converter.catalog.error.PermissionErrorConverter;
import org.jpc.converter.catalog.error.RepresentationErrorConverter;
import org.jpc.converter.catalog.error.ResourceErrorConverter;
import org.jpc.converter.catalog.error.SyntaxErrorConverter;
import org.jpc.converter.catalog.error.SystemErrorConverter;
import org.jpc.converter.catalog.error.TypeErrorConverter;
import org.jpc.converter.catalog.jterm.JTermConverter;
import org.jpc.converter.catalog.jterm.SerializedConverter;
import org.jpc.converter.catalog.list.ArrayConverter;
import org.jpc.converter.catalog.list.CollectionConverter;
import org.jpc.converter.catalog.list.EnumerationConverter;
import org.jpc.converter.catalog.list.IterableConverter;
import org.jpc.converter.catalog.list.IteratorConverter;
import org.jpc.converter.catalog.map.MapConverter.MapToTermConverter;
import org.jpc.converter.catalog.map.MapConverter.TermToMapConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.catalog.primitive.BooleanConverter;
import org.jpc.converter.catalog.primitive.CharacterConverter;
import org.jpc.converter.catalog.primitive.NumberConverter;
import org.jpc.converter.catalog.primitive.StringConverter;
import org.jpc.converter.catalog.termconvertable.TermConvertableConverter;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.error.handling.DefaultJpcErrorHandler;
import org.jpc.error.handling.ErrorHandler;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermManager;
import org.jpc.term.jterm.JTermUtil;
import org.minitoolbox.reflection.IncompatibleTypesException;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

//TODO merge with Jpc class ?
public class DefaultJpc extends Jpc {

	
	private static ConverterManager getDefaultJpcConverterManager(JGum categorizationContext) {
		ConverterManager converterManager = JGumConverterManager.createDefault(categorizationContext);
		
		JpcBuilder.register(converterManager, new TermConvertableConverter()) ; //TODO verify this...

		
		JpcBuilder.register(converterManager, new IsoPrologErrorConverter());
		JpcBuilder.register(converterManager, new DomainErrorConverter());
		JpcBuilder.register(converterManager, new EvaluationErrorConverter());
		JpcBuilder.register(converterManager, new ExistenceErrorConverter());
		JpcBuilder.register(converterManager, new InstantiationErrorConverter());
		JpcBuilder.register(converterManager, new PermissionErrorConverter());
		JpcBuilder.register(converterManager, new RepresentationErrorConverter());
		JpcBuilder.register(converterManager, new ResourceErrorConverter());
		JpcBuilder.register(converterManager, new SyntaxErrorConverter());
		JpcBuilder.register(converterManager, new SystemErrorConverter());
		JpcBuilder.register(converterManager, new TypeErrorConverter());
		
		
		JpcBuilder.register(converterManager, new ArrayConverter());
		JpcBuilder.register(converterManager, new CollectionConverter());
		JpcBuilder.register(converterManager, new EnumerationConverter());
		JpcBuilder.register(converterManager, new IterableConverter());
		JpcBuilder.register(converterManager, new IteratorConverter());
		JpcBuilder.register(converterManager, new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			JpcBuilder.register(converterManager, new TermToMapConverter(mapEntrySeparator));
		}
		
		
		JpcBuilder.register(converterManager, new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			JpcBuilder.register(converterManager, new TermToMapEntryConverter(mapEntrySeparator));
		}
		
		
		JpcBuilder.register(converterManager, new XmlGregorianCalendarConverter());
		JpcBuilder.register(converterManager, new CalendarConverter());
		

		JpcBuilder.register(converterManager, new CharacterConverter());
		JpcBuilder.register(converterManager, new StringConverter());
		JpcBuilder.register(converterManager, new BooleanConverter());
		JpcBuilder.register(converterManager, new NumberConverter());
		
		return converterManager;
	}
	
	
	
	//private final VarConverter nullConverter = new VarConverter();
	private final ChainOfResponsibility<Converter<?,?>,?> fromTermSystemConverter;
	private final JTermManager jTermManager;
	private final ErrorHandler errorHandler;
	//private final JpcPreferences preferences;
	private final TypeSolverManager typeSolverManager; //responsible of recommending types for the result of a conversion.
	
	public DefaultJpc() {
		this(new JGum());
	}
	
	private DefaultJpc(JGum categorizationContext) {
		this(getDefaultJpcConverterManager(categorizationContext), JGumInstantiationManager.createDefault(categorizationContext), JGumTypeSolverManager.createDefault(categorizationContext), JTermUtil.getJTermManager(), new DefaultJpcErrorHandler());
	}
	
	public DefaultJpc(ConverterManager converterManager, InstantiationManager instantiationManager, TypeSolverManager typeSolverManager, JTermManager jTermManager, ErrorHandler errorHandler) {
		super(converterManager, instantiationManager, typeSolverManager);
		this.typeSolverManager = typeSolverManager;
		this.jTermManager = jTermManager;
		this.errorHandler = errorHandler;
		fromTermSystemConverter = getFromTermSystemConverter();
	}
	
	private ChainOfResponsibility<Converter<?,?>,?> getFromTermSystemConverter() {
		List<FromTermConverter<?,?>> systemConverters = Arrays.<FromTermConverter<?,?>>asList(new JTermConverter(), new SerializedConverter());
		return FromTermConverterAdapter.chainConverters((List)systemConverters);
	}
	
	@Override
	public <T> T fromTerm(Term term, Type type) {
		Objects.requireNonNull(term);
		if(!type.equals(Object.class) && TypeWrapper.wrap(type).isAssignableFrom(term.getClass()))
			return (T) term;
			
		try {
			return (T) fromTermSystemConverter.apply(new CheckedConverterEvaluator(term, type, this));
		} catch(ConversionException e) {}
		
		Type termType = getType(term);
		if(termType != null) {
			try {
				type = TypeWrapper.wrap(termType).mostSpecificType(type); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {} //do nothing
		}
//		try {
			return convert(term, type);
//		} catch(ConversionException e) {
//			if(Object.class.equals(type))
//				return (T) term;
//			else
//				throw e;
//		}
	}
	
	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass) {
		if(object==null) {
			if(Var.class.isAssignableFrom(termClass))
				return (T) Var.ANONYMOUS_VAR;
			else
				throw new NullPointerException("A Null object cannot be transformed to a logic term of class " + termClass);
		}
		if(termClass.isAssignableFrom(object.getClass()))
			return (T) object;
		return convert(object, termClass);
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
	public Type getType(Object key, Object object) {
		return typeSolverManager.getType(TypeSolverManager.DEFAULT_KEY, object);
	}
	
}
