package org.jpc.converter;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jconverter.converter.CheckedConverterEvaluator;
import org.jconverter.converter.ConversionException;
import org.jconverter.converter.ConverterManager;
import org.jconverter.converter.ConverterRegister;
import org.jconverter.converter.JGumConverter;
import org.jconverter.converter.JGumConverterManager;
import org.jgum.JGum;
import org.jgum.category.Category;
import org.jgum.category.CategoryProperty.PropertyIterable;
import org.jgum.category.type.TypeCategory;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.converter.catalog.TermConvertableConverter;
import org.jpc.converter.catalog.VarConverter;
import org.jpc.converter.catalog.datetime.CalendarToAtomConverter;
import org.jpc.converter.catalog.datetime.CalendarToNumberTermConverter;
import org.jpc.converter.catalog.datetime.XMLGregorianCalendarConverter;
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
import org.jpc.converter.catalog.primitive.CharacterToNumberTermConverter;
import org.jpc.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.converter.catalog.primitive.ObjectToAtomConverter;
import org.jpc.converter.catalog.primitive.StringToNumberTermConverter;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.IndexManager;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.JpcPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;

public class JpcConverterManager extends JGumConverterManager {

	private static Logger logger = LoggerFactory.getLogger(JpcConverterManager.class);
	
	private static final String CONVERTER_FUNCTOR_NAME = "converter";
	
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
		register(converterManager, new TermConvertableConverter());
		register(converterManager, new VarConverter());
		
		register(converterManager, new CharacterToNumberTermConverter());
		register(converterManager, new ObjectToAtomConverter<Character>(){});
		register(converterManager, new StringToNumberTermConverter());
		register(converterManager, new ObjectToAtomConverter<String>(){});
		register(converterManager, new BooleanConverter());
		//register(converterManager, new NumberConverter());
		register(converterManager, new NumberToNumberTermConverter());
		class NumberToAtomConverter<T extends Number> extends ObjectToAtomConverter<T>{};
		register(converterManager, new NumberToAtomConverter());
		
		register(converterManager, new CalendarToNumberTermConverter());
		register(converterManager, new CalendarToAtomConverter());
		register(converterManager, new XMLGregorianCalendarConverter<Atom>(){});
		class XMLGregorianCalendarConverterToNumberTerm<T extends NumberTerm> extends XMLGregorianCalendarConverter<T>{}
		register(converterManager, new XMLGregorianCalendarConverterToNumberTerm());

		register(converterManager, new ArrayConverter());
		register(converterManager, new CollectionConverter());
		register(converterManager, new EnumerationConverter());
		register(converterManager, new IterableConverter());
		register(converterManager, new IteratorConverter());
		register(converterManager, new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			register(converterManager, new TermToMapConverter(mapEntrySeparator));
		}
		
		register(converterManager, new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			register(converterManager, new TermToMapEntryConverter(mapEntrySeparator));
		}
		
		register(converterManager, new UnknownIsoPrologErrorConverter()); //this should be the first registered error.
		register(converterManager, new DomainErrorConverter());
		register(converterManager, new EvaluationErrorConverter());
		register(converterManager, new ExistenceErrorConverter());
		register(converterManager, new InstantiationErrorConverter());
		register(converterManager, new PermissionErrorConverter());
		register(converterManager, new RepresentationErrorConverter());
		register(converterManager, new ResourceErrorConverter());
		register(converterManager, new SyntaxErrorConverter());
		register(converterManager, new SystemErrorConverter());
		register(converterManager, new TypeErrorConverter());
	}
	
	//TODO move these methods to JpcConverter with Java8 (since then static methods will be allowed in interfaces)
	public static void register(ConverterManager converterManager, JpcConverter converter) {
		if(converter instanceof FromTermConverter)
			registerFromTermConverter(converterManager, (FromTermConverter)converter);
		if(converter instanceof ToTermConverter)
			registerToTermConverter(converterManager, (ToTermConverter)converter);
	}
	
	private static void registerFromTermConverter(ConverterManager converterManager, FromTermConverter<?,?> converter) {
		converterManager.register(FromTermConverterAdapter.forConverter(converter));
	}
	
	private static void registerToTermConverter(ConverterManager converterManager, ToTermConverter<?,?> converter) {
		converterManager.register(ToTermConverterAdapter.forConverter(converter));
	}
	
	private static boolean isValidConvertableTerm(Term term) {
		return term instanceof Compound || term instanceof Atom;
	}
	
	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	
	
	public JpcConverterManager(JGum jgum) {
		super(jgum);
		this.embeddedEngine = new JpcEngine();
		MutableIndexManager indexManager = embeddedEngine.getIndexManager();
		Functor converterFunctor = new Functor(CONVERTER_FUNCTOR_NAME, 2);
		IndexDescriptor indexDescriptor = IndexDescriptor.forIndexedArgument(1, indexManager); //makes use of any index defined for the first argument.
		indexManager.setIndexDescriptor(converterFunctor, indexDescriptor); //clause heads having converter as a functor name will be indexed according to the first argument of the term head.
	}

	public MutableIndexManager getIndexManager() {
		return embeddedEngine.getIndexManager();
	}
	
	public <T> T fromTerm(Term term, Type targetType, Jpc jpc) {
		try {
			return this.<T>evalQuantifiedTermConverter(term, targetType, jpc);
		} catch(ConversionException e) {}
		
		JGumConverter<Term, T> jgumConverter = new JGumConverter<Term, T>(jgum, DEFAULT_KEY) {
			@Override
			protected List<ConverterRegister> getConverters(Class<?> clazz) {
				Category sourceTypeCategory = jgum.forClass(clazz);
				List<TypeCategory<?>> typeCategories = sourceTypeCategory.<TypeCategory<?>>bottomUpCategories();
				typeCategories = new ArrayList<TypeCategory<?>>(Collections2.filter(typeCategories, new Predicate<TypeCategory<?>>() {
					@Override
					public boolean apply(TypeCategory<?> typeCategory) {
						return Term.class.isAssignableFrom(typeCategory.getLabel());
					}
				}));
				List<ConverterRegister> converterRegisters = Lists.newArrayList(new PropertyIterable(typeCategories, key));
				return converterRegisters;
			}
			
		};
		return convert(jgumConverter, term, targetType, jpc);
	}
	
	public <T extends Term> T toTerm(Object object, Class<T> termClass, Jpc jpc) {
		return convert(object, termClass, jpc);
	}
	
	public void register(JpcConverter converter, Term term) {
		if(!isValidConvertableTerm(term))
			throw new JpcException("Term " + term + " cannot be associated with a converter.");
		embeddedEngine.assertz(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, JRef.jref(converter))));
		if(converter instanceof ToTermConverter)
			registerToTermConverter(this, (ToTermConverter)converter);
	}
	
//	public void removeConverters(Term term) {
//		embeddedEngine.retractAll(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, Var.ANONYMOUS_VAR)));
//	}
	
	private <T> T evalQuantifiedTermConverter(Term term, Type targetType, Jpc jpc) {
		T converted = null;
		if(isValidConvertableTerm(term)) {
			String converterVarName = JpcPreferences.JPC_VAR_PREFIX + "Converter";
			Query query = embeddedEngine.query(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, new Var(converterVarName))));
			while(query.hasNext()) {
				Solution solution = query.next();
				Term unifiedTerm = term.replaceVariables(solution);
				FromTermConverter fromTermConverter = (FromTermConverter)((JRef)solution.get(converterVarName)).getRef();
				try {
					converted = (T)new CheckedConverterEvaluator(unifiedTerm, targetType, jpc).apply(FromTermConverterAdapter.forConverter(fromTermConverter));
				} catch(ConversionException e) {} //just try with the next converter.
			}
			query.close();
		}
		if(converted == null)
			throw new ConversionException();
		else
			return converted;
	}
	
}
