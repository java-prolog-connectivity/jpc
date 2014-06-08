package org.jpc.converter;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.ARRAY_FUNCTOR_NAME;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.CLASS_FUNCTOR_NAME;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.TYPE_FUNCTOR_NAME;
import static org.jpc.converter.catalog.reflection.reification.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;

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
import org.jpc.converter.catalog.CustomTermToObjectConverter;
import org.jpc.converter.catalog.JRefToObjectConverter;
import org.jpc.converter.catalog.JpcContextConverter;
import org.jpc.converter.catalog.SequenceConverter;
import org.jpc.converter.catalog.TermConvertableConverter;
import org.jpc.converter.catalog.TermSpecifierConverter;
import org.jpc.converter.catalog.TypedTermToObjectConverter;
import org.jpc.converter.catalog.VarConverter;
import org.jpc.converter.catalog.datetime.CalendarToAtomConverter;
import org.jpc.converter.catalog.datetime.CalendarToNumberTermConverter;
import org.jpc.converter.catalog.datetime.XMLGregorianCalendarConverter;
import org.jpc.converter.catalog.error.DomainErrorConverter;
import org.jpc.converter.catalog.error.EvaluationErrorConverter;
import org.jpc.converter.catalog.error.ExistenceErrorConverter;
import org.jpc.converter.catalog.error.InstantiationErrorConverter;
import org.jpc.converter.catalog.error.JExceptionConverter;
import org.jpc.converter.catalog.error.PermissionErrorConverter;
import org.jpc.converter.catalog.error.RepresentationErrorConverter;
import org.jpc.converter.catalog.error.ResourceErrorConverter;
import org.jpc.converter.catalog.error.StackTraceElementConverter;
import org.jpc.converter.catalog.error.SyntaxErrorConverter;
import org.jpc.converter.catalog.error.SystemErrorConverter;
import org.jpc.converter.catalog.error.TypeErrorConverter;
import org.jpc.converter.catalog.error.UnknownIsoPrologErrorConverter;
import org.jpc.converter.catalog.io.FileConverter;
import org.jpc.converter.catalog.list.ArrayConverter;
import org.jpc.converter.catalog.list.CollectionConverter;
import org.jpc.converter.catalog.list.EnumerationConverter;
import org.jpc.converter.catalog.list.IterableConverter;
import org.jpc.converter.catalog.list.IteratorConverter;
import org.jpc.converter.catalog.map.MapConverter.MapToTermConverter;
import org.jpc.converter.catalog.map.MapConverter.TermToMapConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.catalog.net.URIConverter;
import org.jpc.converter.catalog.primitive.BooleanConverter;
import org.jpc.converter.catalog.primitive.CharacterToNumberTermConverter;
import org.jpc.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.converter.catalog.primitive.ObjectToAtomConverter;
import org.jpc.converter.catalog.primitive.StringToNumberTermConverter;
import org.jpc.converter.catalog.reflection.ConstructorCallConverter;
import org.jpc.converter.catalog.reflection.MethodCallConverter;
import org.jpc.converter.catalog.reflection.ReflectiveClassConverter;
import org.jpc.converter.catalog.reflection.ReflectiveClassConverter.ShortNotationReflectiveClassConverter;
import org.jpc.converter.catalog.reflection.ReflectiveObjectConverter;
import org.jpc.converter.catalog.reflection.reification.ClassConverter;
import org.jpc.converter.catalog.reflection.reification.ClassConverter.ShortNotationClassConverter;
import org.jpc.converter.catalog.reflection.reification.GenericArrayTypeToTermConverter;
import org.jpc.converter.catalog.reflection.reification.ParameterizedTypeConverter;
import org.jpc.converter.catalog.reflection.reification.TermToArrayTypeConverter;
import org.jpc.converter.catalog.reflection.reification.TypeVariableToTermConverter;
import org.jpc.converter.catalog.reflection.reification.WildcardTypeToTermConverter;
import org.jpc.converter.catalog.serialized.FromSerializedConverter;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.engine.prolog.PrologConstants;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.NumberTerm;
import org.jpc.term.SerializedTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.compiler.UncompiledTermException;
import org.jpc.util.JpcPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;

public class JpcConverterManager extends JGumConverterManager {

	private static final Logger logger = LoggerFactory.getLogger(JpcConverterManager.class);
	
	private static final String CONVERTER_FUNCTOR_NAME = "converter";
	
	/**
	 * Registers default Jpc converters in the given converter manager.
	 * @param converterManager a converter manager.
	 * @return the converter manager.
	 */
	public static JpcConverterManager registerDefaults(JpcConverterManager converterManager) {
		ConverterManager.registerDefaults(converterManager); //registering jconverter defaults.
		
		converterManager.register(new JpcContextConverter(), new Functor(JpcContextConverter.JPC_FUNCTOR, 1).asTerm());
		
		converterManager.register(new TermSpecifierConverter(), new Functor(TermSpecifierConverter.TERM_SPECIFIER_FUNCTOR_NAME, 1).asTerm());
		
		converterManager.register(new ReflectiveObjectConverter(), new Functor(ReflectiveObjectConverter.REFLECTIVE_OBJECT_FUNCTOR_NAME, 1).asTerm());
		converterManager.register(new ReflectiveClassConverter(), new Functor(CLASS_FUNCTOR_NAME, 2).asTerm());
		converterManager.register(new ShortNotationReflectiveClassConverter(), new Functor(CLASS_FUNCTOR_NAME, 1).asTerm());
		converterManager.register(new ClassConverter(), new Functor(TYPE_FUNCTOR_NAME, 2).asTerm());
		converterManager.register(new ShortNotationClassConverter(), new Functor(TYPE_FUNCTOR_NAME, 1).asTerm());
		converterManager.register(new ParameterizedTypeConverter(), new Functor(TYPE_FUNCTOR_NAME, 4).asTerm());
		converterManager.register(new TermToArrayTypeConverter(), new Functor(ARRAY_FUNCTOR_NAME, 1).asTerm());
		converterManager.register(new GenericArrayTypeToTermConverter());
		converterManager.register(new TypeVariableToTermConverter(), new Functor(TYPE_VARIABLE_FUNCTOR_NAME, 3).asTerm());
		converterManager.register(new WildcardTypeToTermConverter(), new Functor(TYPE_VARIABLE_FUNCTOR_NAME, 2).asTerm());
		
		
		converterManager.register(new ConstructorCallConverter());
		converterManager.register(new MethodCallConverter(), new Functor(LogtalkConstants.LOGTALK_OPERATOR, 2).asTerm());
		converterManager.register(new SequenceConverter(), new Functor(PrologConstants.SEQUENCE_SEPARATOR, 2).asTerm());
		
		
		converterManager.register(new TypedTermToObjectConverter(), new Functor(TypedTermToObjectConverter.TYPED_TERM_FUNCTOR_NAME, 2).asTerm());
		converterManager.register(new FromSerializedConverter(), new Functor(SerializedTerm.SERIALIZED_TERM_FUNCTOR, 1).asTerm());
		converterManager.register(new CustomTermToObjectConverter(), new Functor(CustomTermToObjectConverter.CUSTOM_TERM_FUNCTOR_NAME, 2).asTerm());
		
		converterManager.register(new TermConvertableConverter());
		converterManager.register(new VarConverter());
		converterManager.register(new JRefToObjectConverter());
		converterManager.register(new CharacterToNumberTermConverter());
		converterManager.register(new ObjectToAtomConverter<Character>(){});
		converterManager.register(new StringToNumberTermConverter());
		converterManager.register(new ObjectToAtomConverter<String>(){});
		converterManager.register(new BooleanConverter());
		//converterManager.register(new NumberConverter());
		converterManager.register(new NumberToNumberTermConverter());
		class NumberToAtomConverter<T extends Number> extends ObjectToAtomConverter<T>{};
		converterManager.register(new NumberToAtomConverter());
		
		converterManager.register(new CalendarToNumberTermConverter());
		converterManager.register(new CalendarToAtomConverter());
		converterManager.register(new XMLGregorianCalendarConverter<Atom>(){});
		class XMLGregorianCalendarConverterToNumberTerm<T extends NumberTerm> extends XMLGregorianCalendarConverter<T>{}
		converterManager.register(new XMLGregorianCalendarConverterToNumberTerm());

		converterManager.register(new ArrayConverter());
		converterManager.register(new CollectionConverter());
		converterManager.register(new EnumerationConverter());
		converterManager.register(new IterableConverter());
		converterManager.register(new IteratorConverter());
		converterManager.register(new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			converterManager.register(new TermToMapConverter(mapEntrySeparator));
		}
		
		converterManager.register(new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			converterManager.register(new TermToMapEntryConverter(mapEntrySeparator));
		}
		
		converterManager.register(new URIConverter(), new Functor(URIConverter.URI_FUNCTOR_NAME,1).asTerm());
		converterManager.register(new FileConverter(), new Functor(FileConverter.FILE_FUNCTOR_NAME,1).asTerm());
		
		
		converterManager.register(new StackTraceElementConverter());
		converterManager.register(new JExceptionConverter());
		
		converterManager.register(new UnknownIsoPrologErrorConverter()); //this should be the first registered error.
		converterManager.register(new DomainErrorConverter());
		converterManager.register(new EvaluationErrorConverter());
		converterManager.register(new ExistenceErrorConverter());
		converterManager.register(new InstantiationErrorConverter());
		converterManager.register(new PermissionErrorConverter());
		converterManager.register(new RepresentationErrorConverter());
		converterManager.register(new ResourceErrorConverter());
		converterManager.register(new SyntaxErrorConverter());
		converterManager.register(new SystemErrorConverter());
		converterManager.register(new TypeErrorConverter());
		
		return converterManager;
	}
	
	
	private static boolean isValidConvertableTerm(Term term) {
		return term instanceof Compound;// || term instanceof Atom;
	}
	
	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	
	
	public JpcConverterManager(JGum jgum) {
		this(jgum, new JpcEngine());
	}

	public JpcConverterManager(JGum jgum, JpcEngine embeddedEngine) {
		super(jgum);
		this.embeddedEngine = embeddedEngine;
		MutableIndexManager indexManager = embeddedEngine.getIndexManager();
		Functor converterFunctor = new Functor(CONVERTER_FUNCTOR_NAME, 2);
		IndexDescriptor indexDescriptor = IndexDescriptor.forIndexedArgument(1, indexManager); //makes use of any index defined for the first argument.
		indexManager.setIndexDescriptor(converterFunctor, indexDescriptor); //clause heads having CONVERTER_FUNCTOR_NAME as a functor name will be indexed according to the first argument of the term head.
	}
	
	public <T> T fromTerm(Term term, Type targetType, Jpc jpc) {
		return fromTerm(DEFAULT_KEY, term, targetType, jpc);
	}
	
	public <T> T fromTerm(Object key, Term term, Type targetType, Jpc jpc) {
		try {
			return this.<T>evalQuantifiedTermConverter(term, targetType, jpc); //the current implementation does not take into consideration the key for finding converters in the embedded Prolog database.
		} catch(ConversionException e) {}
		
		//filtering converters to those only defined in term classes.
		JGumConverter<Term, T> jgumConverter = new JGumConverter<Term, T>(jgum, key) {
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
		return convert(DEFAULT_KEY, object, termClass, jpc);
	}
	
	private <T extends Term> T toTerm(Object key, Object object, Class<T> termClass, Jpc jpc) {
		return convert(object, termClass, jpc);
	}
	
	
	private void registerFromTermConverter(Object key, FromTermConverter<?,?> converter) {
		register(key, FromTermConverterAdapter.forConverter(converter));
	}
	
	private void registerToTermConverter(Object key, ToTermConverter<?,?> converter) {
		register(key, ToTermConverterAdapter.forConverter(converter));
	}
	
	public void register(JpcConverter converter) {
		register(DEFAULT_KEY, converter);
	}
	
	private void register(Object key, JpcConverter converter) {
		if(converter instanceof FromTermConverter)
			registerFromTermConverter(key, (FromTermConverter)converter);
		if(converter instanceof ToTermConverter)
			registerToTermConverter(key, (ToTermConverter)converter);
	}
	
	public void register(JpcConverter converter, Term term) {
		register(DEFAULT_KEY, converter, term);
	}
	
	private void register(Object key, JpcConverter converter, Term term) {
		if(!isValidConvertableTerm(term))
			throw new JpcException("Term " + term + " cannot be associated with a converter.");
		//the current implementation does not take into consideration the key when storing the converter in the Prolog database.
		//an alternative implementation could add the key as another argument to the predicate with functor name CONVERTER_FUNCTOR_NAME.
		embeddedEngine.assertz(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, JRef.jRef(converter))));
		if(converter instanceof ToTermConverter)
			registerToTermConverter(key, (ToTermConverter)converter);
	}
	
//	public void removeConverters(Term term) {
//		embeddedEngine.retractAll(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, Var.ANONYMOUS_VAR)));
//	}
	
	/**
	 * Converts a term to a Java object looking for converters in the embedded Prolog database.
	 * @param term the term to convert.
	 * @param targetType the expected type of the conversion.
	 * @param jpc the context.
	 * @return the conversion of the term to an object.
	 * @throws ConversionException if no converter can be found in the embedded database or if no converter can handle the conversion.
	 */
	private <T> T evalQuantifiedTermConverter(Term term, Type targetType, Jpc jpc) {
		T converted = null;
		boolean conversionFound = false;
		Term unifiedTerm = null;
		if(isValidConvertableTerm(term)) {
			String converterVarName = JpcPreferences.JPC_VAR_PREFIX + "Converter";
			Query query = embeddedEngine.query(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, new Var(converterVarName))));
			while(query.hasNext()) {
				Solution solution = query.next();
				unifiedTerm = term.replaceVariables(solution);
				unifiedTerm = unifiedTerm.compile(true);
				FromTermConverter fromTermConverter = (FromTermConverter)((JRef)solution.get(converterVarName)).getReferent();
				try {
					converted = (T)new CheckedConverterEvaluator(unifiedTerm, targetType, jpc).apply(FromTermConverterAdapter.forConverter(fromTermConverter));
					conversionFound = true;
					break;
				} catch(ConversionException e) {} //just try with the next converter.
			}
			query.close();
		}
		if(!conversionFound)
			throw new ConversionException();
		else {
			try {
				term.unify(unifiedTerm);
			} catch(UncompiledTermException e) {
				//just ignore the exception if the original term is not compiled.
			}
			
			return converted;
		}
	}
	
}
