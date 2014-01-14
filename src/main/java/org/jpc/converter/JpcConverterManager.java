package org.jpc.converter;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jconverter.converter.CheckedConverterEvaluator;
import org.jconverter.converter.ConversionException;
import org.jconverter.converter.ConverterManager;
import org.jconverter.converter.ConverterRegister;
import org.jconverter.converter.JGumConverterManager;
import org.jgum.JGum;
import org.jgum.category.Category;
import org.jgum.category.CategoryProperty.PropertyIterable;
import org.jgum.category.type.TypeCategory;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
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

import com.google.common.base.Optional;
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
		JpcBuilder.register(converterManager, new TermConvertableConverter());
		JpcBuilder.register(converterManager, new VarConverter());
		
		JpcBuilder.register(converterManager, new CharacterToNumberTermConverter());
		JpcBuilder.register(converterManager, new ObjectToAtomConverter<Character>(){});
		JpcBuilder.register(converterManager, new StringToNumberTermConverter());
		JpcBuilder.register(converterManager, new ObjectToAtomConverter<String>(){});
		JpcBuilder.register(converterManager, new BooleanConverter());
		//JpcBuilder.register(converterManager, new NumberConverter());
		JpcBuilder.register(converterManager, new NumberToNumberTermConverter());
		class NumberToAtomConverter<T extends Number> extends ObjectToAtomConverter<T>{};
		JpcBuilder.register(converterManager, new NumberToAtomConverter());
		
		JpcBuilder.register(converterManager, new CalendarToNumberTermConverter());
		JpcBuilder.register(converterManager, new CalendarToAtomConverter());
		JpcBuilder.register(converterManager, new XMLGregorianCalendarConverter<Atom>(){});
		class XMLGregorianCalendarConverterToNumberTerm<T extends NumberTerm> extends XMLGregorianCalendarConverter<T>{}
		JpcBuilder.register(converterManager, new XMLGregorianCalendarConverterToNumberTerm());

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
	
	
	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	
	
	public JpcConverterManager(JGum jgum) {
		super(jgum);
		this.embeddedEngine = new JpcEngine();
		IndexManager indexManager = embeddedEngine.getIndexManager();
		indexManager.setIndexDescriptor( new Functor(CONVERTER_FUNCTOR_NAME, 2), 
				IndexDescriptor.argumentIndexDescriptor(1, indexManager) ); //clause heads having converter as a functor name will be indexed according to the first argument of the term head.
	}

	public IndexManager getIndexManager() {
		return embeddedEngine.getIndexManager();
	}
	
	public <T> T fromTerm(Term term, Type targetType, Jpc jpc) {
		FromTermConverter engineFromTermConverter = null;
		if(term instanceof Compound)
			try {
				return this.<T>evalQuantifiedTermConverter((Compound)term, targetType, jpc);
			} catch(ConversionException e) {}
		
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
	
	public void registerConverter(Compound term, FromTermConverter<Compound, ?> fromTermConverter) {
		embeddedEngine.assertz(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, new JRef(fromTermConverter))));
	}
	
	public void removeConverters(Compound term) {
		embeddedEngine.retractAll(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, Var.ANONYMOUS_VAR)));
	}
	
	private <T> T evalQuantifiedTermConverter(Compound term, Type targetType, Jpc jpc) {
		FromTermConverter fromTermConverter = null;
		String converterVarName = JpcPreferences.JPC_VAR_PREFIX + "Converter";
		Query query = embeddedEngine.query(new Compound(CONVERTER_FUNCTOR_NAME, asList(term, new Var(converterVarName))));
		Optional<Solution> solutionOpt = query.oneSolution();
		if(solutionOpt.isPresent()) {
			Solution solution = solutionOpt.get();
			Term unifiedTerm = term.replaceVariables(solution);
			fromTermConverter = (FromTermConverter)((JRef)solution.get(converterVarName)).getRef();
			return (T)new CheckedConverterEvaluator(unifiedTerm, targetType, jpc).apply(FromTermConverterAdapter.forConverter(fromTermConverter));
		}
		throw new ConversionException();
	}
	
}
