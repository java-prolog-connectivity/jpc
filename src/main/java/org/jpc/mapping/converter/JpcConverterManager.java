package org.jpc.mapping.converter;

import static com.google.common.collect.Lists.newArrayList;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.mapping.converter.catalog.net.UriConverter.URI_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.reflection.type.ReificationConstants.ARRAY_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.reflection.type.ReificationConstants.CLASS_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.reflection.type.ReificationConstants.TYPE_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.reflection.type.ReificationConstants.TYPE_VARIABLE_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.util.OptionalConverter.OPTIONAL_FUNCTOR_NAME;
import static org.jpc.mapping.converter.catalog.util.UuidConverter.UUID_FUNCTOR_NAME;
import static org.jpc.term.Functor.functor;
import static org.jpc.term.JRef.jRef;
import static org.jpc.term.Var.var;

import java.util.List;

import org.jcategory.JCategory;
import org.jcategory.category.Category;
import org.jcategory.category.CategoryProperty.PropertyIterable;
import org.jcategory.category.Key;
import org.jcategory.category.type.TypeCategory;
import org.jconverter.converter.ConversionFunction;
import org.jconverter.converter.Converter;
import org.jconverter.converter.ConverterImpl;
import org.jconverter.converter.ConverterKey;
import org.jconverter.converter.ConverterManager;
import org.jconverter.converter.ConverterRegister;
import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.InterTypeConverterEvaluator;
import org.jconverter.converter.InterTypeConverterManager;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.JpcException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.engine.logtalk.LogtalkConstants;
import org.jpc.engine.prolog.PrologConstants;
import org.jpc.mapping.converter.catalog.CustomTermToObjectConverter;
import org.jpc.mapping.converter.catalog.JRefToObjectConverter;
import org.jpc.mapping.converter.catalog.JpcContextConverter;
import org.jpc.mapping.converter.catalog.SequenceConverter;
import org.jpc.mapping.converter.catalog.TermConvertableConverter;
import org.jpc.mapping.converter.catalog.TermSpecifierConverter;
import org.jpc.mapping.converter.catalog.TypedTermToObjectConverter;
import org.jpc.mapping.converter.catalog.VarConverter;
import org.jpc.mapping.converter.catalog.collection.ArrayConverter;
import org.jpc.mapping.converter.catalog.collection.CollectionConverter;
import org.jpc.mapping.converter.catalog.collection.EnumerationConverter;
import org.jpc.mapping.converter.catalog.collection.IterableConverter;
import org.jpc.mapping.converter.catalog.collection.IteratorConverter;
import org.jpc.mapping.converter.catalog.datetime.CalendarToAtomConverter;
import org.jpc.mapping.converter.catalog.datetime.CalendarToNumberTermConverter;
import org.jpc.mapping.converter.catalog.datetime.TimestampToNumberTermConverter;
import org.jpc.mapping.converter.catalog.datetime.XMLGregorianCalendarConverter;
import org.jpc.mapping.converter.catalog.error.DomainErrorConverter;
import org.jpc.mapping.converter.catalog.error.EvaluationErrorConverter;
import org.jpc.mapping.converter.catalog.error.ExistenceErrorConverter;
import org.jpc.mapping.converter.catalog.error.InstantiationErrorConverter;
import org.jpc.mapping.converter.catalog.error.PermissionErrorConverter;
import org.jpc.mapping.converter.catalog.error.RepresentationErrorConverter;
import org.jpc.mapping.converter.catalog.error.ResourceErrorConverter;
import org.jpc.mapping.converter.catalog.error.StackTraceElementConverter;
import org.jpc.mapping.converter.catalog.error.SyntaxErrorConverter;
import org.jpc.mapping.converter.catalog.error.SystemErrorConverter;
import org.jpc.mapping.converter.catalog.error.ThrowableConverter;
import org.jpc.mapping.converter.catalog.error.TypeErrorConverter;
import org.jpc.mapping.converter.catalog.error.UnknownIsoPrologErrorConverter;
import org.jpc.mapping.converter.catalog.io.FileConverter;
import org.jpc.mapping.converter.catalog.map.MapConverter.MapToTermConverter;
import org.jpc.mapping.converter.catalog.map.MapConverter.TermToMapConverter;
import org.jpc.mapping.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.mapping.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.mapping.converter.catalog.net.UriConverter;
import org.jpc.mapping.converter.catalog.primitive.BooleanConverter;
import org.jpc.mapping.converter.catalog.primitive.CharSequenceToTermConverter;
import org.jpc.mapping.converter.catalog.primitive.CharacterToNumberTermConverter;
import org.jpc.mapping.converter.catalog.primitive.NumberToNumberTermConverter;
import org.jpc.mapping.converter.catalog.primitive.ObjectToAtomConverter;
import org.jpc.mapping.converter.catalog.primitive.StringToNumberTermConverter;
import org.jpc.mapping.converter.catalog.reflection.ConstructorCallConverter;
import org.jpc.mapping.converter.catalog.reflection.EnumConverter;
import org.jpc.mapping.converter.catalog.reflection.MethodCallConverter;
import org.jpc.mapping.converter.catalog.reflection.ReflectiveClassConverter;
import org.jpc.mapping.converter.catalog.reflection.ReflectiveClassConverter.ShortNotationReflectiveClassConverter;
import org.jpc.mapping.converter.catalog.reflection.ReflectiveObjectConverter;
import org.jpc.mapping.converter.catalog.reflection.type.ClassConverter;
import org.jpc.mapping.converter.catalog.reflection.type.ClassConverter.ShortNotationClassConverter;
import org.jpc.mapping.converter.catalog.reflection.type.GenericArrayTypeToTermConverter;
import org.jpc.mapping.converter.catalog.reflection.type.ParameterizedTypeConverter;
import org.jpc.mapping.converter.catalog.reflection.type.TermToArrayTypeConverter;
import org.jpc.mapping.converter.catalog.reflection.type.TypeVariableToTermConverter;
import org.jpc.mapping.converter.catalog.reflection.type.WildcardTypeToTermConverter;
import org.jpc.mapping.converter.catalog.serialized.FromSerializedConverter;
import org.jpc.mapping.converter.catalog.util.OptionalConverter;
import org.jpc.mapping.converter.catalog.util.UuidConverter;
import org.jpc.mapping.typesolver.catalog.MapTypeSolver;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.Number;
import org.jpc.term.SerializedTerm;
import org.jpc.term.Term;
import org.jpc.term.compiler.UncompiledTermException;
import org.jpc.util.JpcPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JpcConverterManager extends InterTypeConverterManager {

	private static final Logger logger = LoggerFactory.getLogger(JpcConverterManager.class);
	
	static final String FROM_TERM_CONVERTER_FUNCTOR_NAME = "from_term_converter";
	
	/**
	 * Registers default Jpc converters in the given converter manager.
	 * @param converterManager a converter manager.
	 * @return the converter manager.
	 */
	public static JpcConverterManager registerDefaults(JpcConverterManager converterManager) {
		ConverterManager.registerDefaults(converterManager); //registering jconverter defaults.
		
		converterManager.register(new JpcContextConverter(), functor(JpcContextConverter.JPC_FUNCTOR, 1));
		
		converterManager.register(new TermSpecifierConverter(), functor(TermSpecifierConverter.TERM_SPECIFIER_FUNCTOR_NAME, 1));

		converterManager.register(new EnumConverter(), functor(EnumConverter.ENUM_FUNCTOR_NAME, 2));
		converterManager.register(new ReflectiveObjectConverter(), functor(ReflectiveObjectConverter.REFLECTIVE_OBJECT_FUNCTOR_NAME, 1));
		converterManager.register(new ReflectiveClassConverter(), functor(CLASS_FUNCTOR_NAME, 2));
		converterManager.register(new ShortNotationReflectiveClassConverter(), functor(CLASS_FUNCTOR_NAME, 1));
		converterManager.register(new ClassConverter(), functor(TYPE_FUNCTOR_NAME, 2));
		converterManager.register(new ShortNotationClassConverter(), functor(TYPE_FUNCTOR_NAME, 1));
		converterManager.register(new ParameterizedTypeConverter(), functor(TYPE_FUNCTOR_NAME, 4));
		converterManager.register(new TermToArrayTypeConverter(), functor(ARRAY_FUNCTOR_NAME, 1));
		converterManager.register(new GenericArrayTypeToTermConverter());
		converterManager.register(new TypeVariableToTermConverter(), functor(TYPE_VARIABLE_FUNCTOR_NAME, 3));
		converterManager.register(new WildcardTypeToTermConverter(), functor(TYPE_VARIABLE_FUNCTOR_NAME, 2));
		
		
		converterManager.register(new ConstructorCallConverter());
		converterManager.register(new MethodCallConverter(), functor(LogtalkConstants.LOGTALK_OPERATOR, 2));
		converterManager.register(new SequenceConverter(), functor(PrologConstants.SEQUENCE_SEPARATOR, 2));
		
		
		converterManager.register(new TypedTermToObjectConverter(), functor(TypedTermToObjectConverter.TYPED_TERM_FUNCTOR_NAME, 2));
		converterManager.register(new FromSerializedConverter(), functor(SerializedTerm.SERIALIZED_TERM_FUNCTOR, 1));
		converterManager.register(new CustomTermToObjectConverter(), functor(CustomTermToObjectConverter.CUSTOM_TERM_FUNCTOR_NAME, 2));
		
		converterManager.register(new TermConvertableConverter());
		converterManager.register(new VarConverter());
		converterManager.register(new JRefToObjectConverter());
		converterManager.register(new UuidConverter(), functor(UUID_FUNCTOR_NAME, 1));
		converterManager.register(new OptionalConverter(), functor(OPTIONAL_FUNCTOR_NAME, 1));
		converterManager.register(new CharacterToNumberTermConverter());
		converterManager.register(new ObjectToAtomConverter<Character>(){});
		converterManager.register(new StringToNumberTermConverter());
		converterManager.register(new CharSequenceToTermConverter());
		converterManager.register(new ObjectToAtomConverter<String>(){});
		converterManager.register(new BooleanConverter());
		//converterManager.register(new NumberConverter());
		converterManager.register(new NumberToNumberTermConverter());
		class NumberToAtomConverter<T extends java.lang.Number> extends ObjectToAtomConverter<T>{};
		converterManager.register(new NumberToAtomConverter());
		
		converterManager.register(new CalendarToNumberTermConverter());
		converterManager.register(new CalendarToAtomConverter());
		converterManager.register(new XMLGregorianCalendarConverter<Atom>(){});
		class XMLGregorianCalendarConverterToNumberTerm<T extends Number> extends XMLGregorianCalendarConverter<T>{}
		converterManager.register(new XMLGregorianCalendarConverterToNumberTerm());
		converterManager.register(new TimestampToNumberTermConverter());

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
		
		converterManager.register(new UriConverter(), functor(URI_FUNCTOR_NAME,1));
		converterManager.register(new FileConverter(), functor(FileConverter.FILE_FUNCTOR_NAME,1));
		
		
		converterManager.register(new StackTraceElementConverter());
		converterManager.register(new ThrowableConverter());
		
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
	

	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	private final FromTermConverter<Term, Object> fromUnifiableTermConverter = new FromUnifiableTermConverter();
	private final ToTermConverter<Object, Term> toUnifiableTermConverter = new ToUnifiableTermConverter();

	public JpcConverterManager(JCategory categorization) {
		this(categorization, new JpcEngine());
	}

	public JpcConverterManager(JCategory categorization, JpcEngine embeddedEngine) {
		super(categorization);
		this.embeddedEngine = embeddedEngine;
		MutableIndexManager indexManager = embeddedEngine.getIndexManager();
		Functor converterFunctor = functor(FROM_TERM_CONVERTER_FUNCTOR_NAME, 2);
		IndexDescriptor indexDescriptor = IndexDescriptor.forIndexedArgument(1, indexManager); //makes use of any index defined for the first argument.
		indexManager.setIndexDescriptor(converterFunctor, indexDescriptor); //clause heads having FROM_TERM_CONVERTER_FUNCTOR_NAME as a functor name will be indexed according to the first argument of the term head.
	}
	
/*	public <T> T fromTerm(Term term, TypeDomain target, Jpc jpc) {
		return fromTerm(ConverterKey.DEFAULT_KEY, term, target, jpc);
	}*/
	
	public <T> T fromTerm(Key key, Term term, TypeDomain target, Jpc jpc) {
		try {
			return (T) fromUnifiableTermConverter.fromTerm(term, target, jpc); //the current implementation does not take into consideration the key for finding converters in the embedded Prolog database.
		} catch(DelegateConversionException e) {}
		
		//filtering converters to those only defined in term classes.
		Converter<Term, T> converter = new ConverterImpl<Term, T>(categorization, key) {
			@Override
			protected List<ConverterRegister> getConverters(Class<?> clazz) {
				Category sourceTypeCategory = categorization.forClass(clazz);
				List<TypeCategory<?>> typeCategories = sourceTypeCategory.<TypeCategory<?>>bottomUpCategories();
				typeCategories = typeCategories.stream().filter(typeCategory -> Term.class.isAssignableFrom(typeCategory.getLabel())).collect(toList());
				List<ConverterRegister> converterRegisters = newArrayList(new PropertyIterable(typeCategories, key));
				return converterRegisters;
			}
		};
		return convert(converter, term, target, jpc);
	}
	
/*	public <T extends Term> T toTerm(Object object, TypeDomain target, Jpc jpc) {
		return convert(ConverterKey.DEFAULT_KEY, object, target, jpc);
	}*/
	
	public <T extends Term> T toTerm(Key key, Object object, TypeDomain target, Jpc jpc) {
		return convert(key, object, target, jpc);
	}
	
	
	private void registerFromTermConverter(Key key, FromTermConverter<?,?> converter) {
		register(key, Adapters.asConversionFunction(converter));
	}
	
	private void registerToTermConverter(Key key, ToTermConverter<?,?> converter) {
		register(key, Adapters.asConversionFunction(converter));
	}
	
	public void register(JpcConverter converter) {
		register(ConverterKey.DEFAULT_KEY, converter);
	}
	
	public void register(Key key, JpcConverter converter) {
		if (converter instanceof FromTermConverter) {
			registerFromTermConverter(key, (FromTermConverter) converter);
		}
		if (converter instanceof ToTermConverter) {
			registerToTermConverter(key, (ToTermConverter) converter);
		}
	}

	public void register(JpcConverter converter, Functor functor) {
		register(converter, functor.asTerm());
	}

	public void register(JpcConverter converter, Term term) {
		register(ConverterKey.DEFAULT_KEY, converter, term);
	}

	static boolean isValidConvertableTerm(Term term) {
		return term instanceof Compound;// || term instanceof Atom;
	}

	private void register(Key key, JpcConverter converter, Term term) {
		if (!isValidConvertableTerm(term)) {
			throw new JpcException("Term " + term + " cannot be associated with a converter.");
		}

		//the current implementation does not take into consideration the key when storing a converter in the Prolog database.
		//an alternative implementation could add the key as another argument to the predicate wrapping the converter.

		if (converter instanceof FromTermConverter) {
			embeddedEngine.assertz(new Compound(FROM_TERM_CONVERTER_FUNCTOR_NAME, asList(
					term, jRef(Adapters.asConversionFunction((FromTermConverter) converter)))));
		}
		if (converter instanceof ToTermConverter) {
			registerToTermConverter(key, (ToTermConverter) converter); //delete
		}
	}
	
/*	public void removeConverters(Term term) {
		embeddedEngine.retractAll(compound(FROM_TERM_CONVERTER_FUNCTOR_NAME, asList(term, dontCare())));
		embeddedEngine.retractAll(compound(TO_TERM_CONVERTER_FUNCTOR_NAME, asList(term, dontCare())));
	}*/




	private class ToUnifiableTermConverter implements ToTermConverter<Object, Term> {

		@Override
		public Term toTerm(Object object, TypeDomain target, Jpc context) {
			throw new UnsupportedOperationException();
		}
	}

	private class FromUnifiableTermConverter implements FromTermConverter<Term, Object> {
		/**
		 * Converts a term to a Java object looking for converters in the embedded Prolog database.
		 * @param term the term to convert.
		 * @param target the expected type of the conversion.
		 * @param jpc the context.
		 * @return the conversion of the term to an object.
		 * @throws DelegateConversionException if no converter can be found in the embedded database or if no converter can handle the conversion.
		 */
		@Override
		public Object fromTerm(Term term, TypeDomain target, Jpc jpc) {
			Object converted = null;
			boolean converterFound = false;
			Term unifiedTerm = null;
			if (JpcConverterManager.isValidConvertableTerm(term)) {
				String converterVarName = JpcPreferences.JPC_VAR_PREFIX + "Converter";
				Query query = embeddedEngine.query(new Compound(JpcConverterManager.FROM_TERM_CONVERTER_FUNCTOR_NAME, asList(term, var(converterVarName))));
				while (query.hasNext()) {
					Solution solution = query.next();
					unifiedTerm = term.replaceVariables(solution);
					unifiedTerm = unifiedTerm.compile(true);
					ConversionFunction converter = (ConversionFunction) ((JRef) solution.get(converterVarName)).getReferent();
					try {
						converted = new InterTypeConverterEvaluator(conversionGoal(unifiedTerm, target), jpc).apply(converter);
						converterFound = true;
						break;
					} catch (DelegateConversionException e) {} //just try with the next converter.
				}
				query.close();
			}
			if (!converterFound) {
				throw new DelegateConversionException(conversionGoal(term, target));
			} else {
				try {
					term.unify(unifiedTerm);
				} catch (UncompiledTermException e) {
					//just ignore the exception if the original term is not compiled.
				}
				return converted;
			}
		}
	}
	
}
