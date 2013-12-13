package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jconverter.converter.ConverterManager;
import org.jconverter.converter.ConverterRegister;
import org.jconverter.converter.JGumConverterManager;
import org.jgum.JGum;
import org.jgum.category.Category;
import org.jgum.category.CategoryProperty.PropertyIterable;
import org.jgum.category.type.TypeCategory;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.catalog.datetime.CalendarConverter;
import org.jpc.converter.catalog.datetime.XmlGregorianCalendarConverter;
import org.jpc.converter.catalog.error.DomainErrorConverter;
import org.jpc.converter.catalog.error.EvaluationErrorConverter;
import org.jpc.converter.catalog.error.ExistenceErrorConverter;
import org.jpc.converter.catalog.error.InstantiationErrorConverter;
import org.jpc.converter.catalog.error.PermissionErrorConverter;
import org.jpc.converter.catalog.error.RepresentationErrorConverter;
import org.jpc.converter.catalog.error.ResourceErrorConverter;
import org.jpc.converter.catalog.error.SyntaxErrorConverter;
import org.jpc.converter.catalog.error.SystemErrorConverter;
import org.jpc.converter.catalog.error.TypeErrorConverter;
import org.jpc.converter.catalog.error.UnknownIsoPrologErrorConverter;
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
import org.jpc.converter.catalog.var.VarConverter;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;

public class JpcConverterManager extends JGumConverterManager {

	private static Logger logger = LoggerFactory.getLogger(JpcConverterManager.class);
	
	/**
	 * @param jgum a JGum categorization context.
	 * @return an instance of JpcConverterManager configured with default type solvers.
	 */
	public static JpcConverterManager createDefault(JGum jgum) {
		JpcConverterManager converterManager = new JpcConverterManager(jgum);
		registerDefaults(converterManager); //registering jconverter defaults.
		registerJpcDefaults(converterManager); //registering jpcconverter defaults.
		return converterManager;
	}
	
	/**
	 * Registers default Jpc converters in the given converter manager.
	 * @param converterManager a converter manager.
	 */
	public static void registerJpcDefaults(ConverterManager converterManager) {
		JpcBuilder.register(converterManager, new TermConvertableConverter());
		JpcBuilder.register(converterManager, new VarConverter());
		
		JpcBuilder.register(converterManager, new CharacterConverter());
		JpcBuilder.register(converterManager, new StringConverter());
		JpcBuilder.register(converterManager, new BooleanConverter());
		JpcBuilder.register(converterManager, new NumberConverter());
		
		JpcBuilder.register(converterManager, new XmlGregorianCalendarConverter());
		JpcBuilder.register(converterManager, new CalendarConverter());

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
		
		JpcBuilder.register(converterManager, new UnknownIsoPrologErrorConverter()); //this should be the first registered error.
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
	}
	
	public JpcConverterManager(JGum jgum) {
		super(jgum);
	}

	public <T> T fromTerm(Term term, Type targetType, Jpc jpc) {
		Category sourceTypeCategory = jgum.forClass(term.getClass());
		List<TypeCategory<?>> typeCategories = sourceTypeCategory.<TypeCategory<?>>bottomUpCategories();
		typeCategories = new ArrayList<TypeCategory<?>>(Collections2.filter(typeCategories, new Predicate<TypeCategory<?>>() {
			@Override
			public boolean apply(TypeCategory<?> typeCategory) {
				return Term.class.isAssignableFrom(typeCategory.getLabel());
			}
		}));
		List<ConverterRegister> converterRegisters = Lists.newArrayList(new PropertyIterable(typeCategories, DEFAULT_KEY));
		return evalConverters(converterRegisters, term, targetType, jpc);
	}
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass, Jpc jpc) {
		return convert(object, termClass, jpc);
	}
	
	
	/*
	
	
	private List<Converter<?,?>> converters;
	
	public JpcConverterManager() {
		converters = new ArrayList<Converter<?,?>>();
	}
	
	public void register(Converter<?,?> converter) {
		converters.add(0, converter);
	}
	
	public void registerLast(Converter<?,?> converter) {
		converters.add(converter);
	}

	@Override
	public ObjectType fromTerm(TermType term, Type type, Jpc context) {
		TypeWrapper typeWrapper = TypeWrapper.wrap(type);

		for(Converter converter : converters) {
			Type bestTypeForConverter;
			Type converterObjectType = converter.getObjectType();
		
			try {
				bestTypeForConverter = typeWrapper.mostSpecificType(converterObjectType); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {
				continue; //do nothing, just try the next converter
			} 

			if(converter.canConvertFromTerm(term, bestTypeForConverter)) {
				try {
					//this check is because in the call to fromTerm/2 the type argument would be lost. If the type is more specific than the converter type, a call to fromTerm/3 is more appropriate.
					if(bestTypeForConverter.equals(converterObjectType)) { 
						return (ObjectType) converter.fromTerm(term, context); //by default will delegate to fromTerm/3 unless overridden by the programmer
					} else {
						return (ObjectType) converter.fromTerm(term, bestTypeForConverter, context); //the idea of these two alternatives is to allow the programmer to override either fromTerm/2 (if the type argument is not necessary) or fromTerm/3 (more verbose)
					}
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion from term to objects
						ConversionException e){//converters should throw this exception if they are not able to convert a term to a Java object.
					//just try the next converter if any of these exceptions occurs
				}
			}
		}
		throw new ConversionException(term.toString(), type.toString()); //no converter was able to convert the term to the desired type
	}
	
	@Override
	public <T extends TermType> T toTerm(ObjectType object, Class<T> termClass, Jpc context) {
		TypeWrapper termClassWrapper = TypeWrapper.wrap(termClass);
		
		for(Converter converter : converters) {
			Class<? extends Term> bestTermClassForConverter;
			Type converterTermType = converter.getTermType();
			
			try {
				bestTermClassForConverter = (Class<? extends Term>)termClassWrapper.mostSpecificType(converterTermType); //will throw an exception if the types are not compatible
			} catch(IncompatibleTypesException e) {
				continue; //do nothing, just try the next converter
			}
			
			if(converter.canConvertToTerm(object, bestTermClassForConverter)) {
				try {
					if(bestTermClassForConverter.equals(converterTermType)) {
						return (T) converter.toTerm(object, context);
					} else {
						return (T) converter.toTerm(object, bestTermClassForConverter, context);
					}
				} catch(UnsupportedOperationException|//exception thrown if the converter does not support conversion to terms
						ConversionException e) { //exception thrown if the converter finds that it is unable to convert certain object to a term.
					//just try the next converter if any of these exceptions occurs
				} 
			}
		}
		throw new ConversionException(object.toString(), Term.class.getName());
	}
	*/
}
