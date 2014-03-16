package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;
import static org.jpc.Jpc.defaultJpc;
import static org.jpc.engine.logtalk.LogtalkConstants.ABOLISH_CATEGORY;
import static org.jpc.engine.logtalk.LogtalkConstants.ABOLISH_EVENTS;
import static org.jpc.engine.logtalk.LogtalkConstants.ABOLISH_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.ABOLISH_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.CATEGORY_PROPERTY;
import static org.jpc.engine.logtalk.LogtalkConstants.COMPLEMENTS_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.CONFORMS_TO_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.CREATE_CATEGORY;
import static org.jpc.engine.logtalk.LogtalkConstants.CREATE_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.CREATE_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_CATEGORY;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_EVENT;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_LOGTALK_FLAG;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_OBJECT;
import static org.jpc.engine.logtalk.LogtalkConstants.CURRENT_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.DEFINE_EVENTS;
import static org.jpc.engine.logtalk.LogtalkConstants.EXTENDS_CATEGORY;
import static org.jpc.engine.logtalk.LogtalkConstants.EXTENDS_OBJECTS;
import static org.jpc.engine.logtalk.LogtalkConstants.EXTENDS_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.IMPLEMENTS_PROTOCOL;
import static org.jpc.engine.logtalk.LogtalkConstants.IMPORTS_CATEGORY;
import static org.jpc.engine.logtalk.LogtalkConstants.INSTANTIATES_CLASS;
import static org.jpc.engine.logtalk.LogtalkConstants.LOGTALK_LOAD;
import static org.jpc.engine.logtalk.LogtalkConstants.OBJECT_PROPERTY;
import static org.jpc.engine.logtalk.LogtalkConstants.PROTOCOL_PROPERTY;
import static org.jpc.engine.logtalk.LogtalkConstants.SET_LOGTALK_FLAG;
import static org.jpc.engine.logtalk.LogtalkConstants.SPECIALIZES_CLASS;
import static org.jpc.term.ListTerm.listTerm;

import java.io.File;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineProxy;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.reflect.TypeToken;

public class LogtalkEngine extends PrologEngineProxy {

	private static final Logger logger = LoggerFactory.getLogger(LogtalkEngine.class);
	
	public LogtalkEngine(PrologEngine prologEngine) {
		super(prologEngine);
	}
	
	public boolean logtalkLoad(List<? extends Term> terms) {
		return query(new Compound(LOGTALK_LOAD, asList(listTerm(terms)))).hasSolution(); 
	}
	
	public boolean logtalkLoad(Term... terms) {
		return logtalkLoad(asList(terms));
	}
	
	public boolean logtalkLoad(String... resources) {
		Type targetType = new TypeToken<List<Atom>>(){}.getType();
		return logtalkLoad(defaultJpc().<List<? extends Term>>convert(resources, targetType));
	}
	
	public String currentLogtalkFlag(LogtalkFlag flag) {
		String flagValue = null;
		Var varFlag = new Var("Var");
		Map<String, Term> solutions = query(new Compound(CURRENT_LOGTALK_FLAG, Arrays.asList(flag.asTerm(), varFlag))).oneSolutionOrThrow();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlag.getName());
			flagValue = flagValueTerm.getName();
		}
		return flagValue;
	}
	
	public boolean setLogtalkFlag(LogtalkFlag flag, String value) {
		return query(new Compound(SET_LOGTALK_FLAG, asList(new Atom(flag.toString()), new Atom(value)))).hasSolution();
	}
	
	public Map<String, LogtalkLibrary> getLibraries() {
		Map<String, LogtalkLibrary> libraries = new HashMap<>();
		Query query = query("logtalk_library_path(Alias,TermPath), logtalk::expand_library_path(Alias, Dir)");
		for(Solution oneSolution : query.allSolutions()) {
			String alias = oneSolution.getString("Alias");
			Term termPath = oneSolution.get("TermPath");
			String absolutePath = oneSolution.getString("Dir");
			File dir = new File(absolutePath);
			LogtalkLibraryDescription libraryDescription = new LogtalkLibraryDescription(alias, termPath, dir);
			LogtalkLibrary library = new LogtalkLibrary(libraryDescription);
			libraries.put(alias, library);
		}
		return libraries;
	}
	
	public Query currentObject(Term term) {
		return query(new Compound(CURRENT_OBJECT, asList(term)));
	}
	
	public List<LogtalkObject> currentObjects() {
		List<LogtalkObject> currentObjects = new ArrayList<>();
		Var logtalkObjectVar = new Var("LogtalkObject");
		Compound compound = new Compound(CURRENT_OBJECT, asList(logtalkObjectVar));
		for(Map<String, Term> solution : query(compound).allSolutions()) {
			Term currentObjectTerm = solution.get(logtalkObjectVar.getName());
			currentObjects.add(new LogtalkObject(currentObjectTerm, this));
		}
		return currentObjects;
	}
	
	/**
	 * 
	 * @param objectName
	 * @return a list of arities of all the Logtalk objects in the logic side having as id the parameter of the Java method
	 */
	//currently assuming that the cardinalities of the objects in the logtalk side are returned ordered from the lowest to the highest
	public List<Integer> objectArities(String objectName) {
		List<LogtalkObject> currentObjects = currentObjects();
		List<Integer> arities = new ArrayList<>();
		for(LogtalkObject currentObject: currentObjects) {
			Term name = currentObject.name();
			if(name instanceof Atom && ((Atom)name).getName().equals(objectName)) {
				arities.add(currentObject.arity());
			}
		}
		return arities;
	}

	public Query createObject(Term object, Term relations, Term directives, Term Clauses) {
		return query(new Compound(CREATE_OBJECT, asList(object, relations, directives, Clauses)));
	}
	
	public Query abolishObject(Term object) {
		return query(new Compound(ABOLISH_OBJECT, asList(object)));
	}
	
	public Query instantiatesClass(Term instance, Term clazz) {
		return query(new Compound(INSTANTIATES_CLASS, asList(instance, clazz)));
	}
	
	public Query instantiatesClass(Term instance, Term clazz, Term scope) {
		return query(new Compound(INSTANTIATES_CLASS, asList(instance, clazz, scope)));
	}
	
	public Query specializesClass(Term clazz, Term superClass) {
		return query(new Compound(SPECIALIZES_CLASS, asList(clazz, superClass)));
	}
	
	public Query specializesClass(Term clazz, Term superClass, Term scope) {
		return query(new Compound(SPECIALIZES_CLASS, asList(clazz, superClass, scope)));
	}
	
	public Query extendsObject(Term child, Term parent) {
		return query(new Compound(EXTENDS_OBJECTS, asList(child, parent)));
	}
	
	public Query extendsObject(Term child, Term parent, Term scope) {
		return query(new Compound(EXTENDS_OBJECTS, asList(child, parent, scope)));
	}
	
	public Query importsCategory(Term object, Term category) {
		return query(new Compound(IMPORTS_CATEGORY, asList(object, category)));
	}
	
	public Query importsCategory(Term object, Term category, Term scope) {
		return query(new Compound(IMPORTS_CATEGORY, asList(object, category, scope)));
	}
	
	public Query implementsProtocol(Term object, Term protocol) {
		return query(new Compound(IMPLEMENTS_PROTOCOL, asList(object, protocol)));
	}
	
	public Query implementsProtocol(Term object, Term protocol, Term scope) {
		return query(new Compound(IMPLEMENTS_PROTOCOL, asList(object, protocol, scope)));
	}
	
	public Query conformsToProtocol(Term object, Term protocol) {
		return query(new Compound(CONFORMS_TO_PROTOCOL, asList(object, protocol)));
	}
	
	public Query conformsToProtocol(Term object, Term protocol, Term scope) {
		return query(new Compound(CONFORMS_TO_PROTOCOL, asList(object, protocol, scope)));
	}
	
	public Query complementsObject(Term category, Term object) {
		return query(new Compound(COMPLEMENTS_OBJECT, asList(category, object)));
	}
	
	public Query objectProperty(Term object, Term property) {
		return query(new Compound(OBJECT_PROPERTY, asList(object, property)));
	}
	
	
	public Query currentProtocol(Term protocol) {
		return query(new Compound(CURRENT_PROTOCOL, asList(protocol)));
	}
	
	public Query createProtocol(Term protocol, Term relations, Term directives) {
		return query(new Compound(CREATE_PROTOCOL, asList(protocol, relations, directives)));
	}
	
	public Query abolishProtocol(Term protocol) {
		return query(new Compound(ABOLISH_PROTOCOL, asList(protocol)));
	}
	
	public Query extendsProtocol(Term child, Term parent) {
		return query(new Compound(EXTENDS_PROTOCOL, asList(child, parent)));
	}
	
	public Query extendsProtocol(Term child, Term parent, Term scope) {
		return query(new Compound(EXTENDS_PROTOCOL, asList(child, parent, scope)));
	}
	
	public Query protocolProperty(Term protocol, Term property) {
		return query(new Compound(PROTOCOL_PROPERTY, asList(protocol, property)));
	}
	
	
	public Query currentCategory(Term category) {
		return query(new Compound(CURRENT_CATEGORY, asList(category)));
	}
	
	public Query createCategory(Term category, Term relations, Term directives, Term Clauses) {
		return query(new Compound(CREATE_CATEGORY, asList(category, relations, directives, Clauses)));
	}
	
	public Query abolishCategory(Term category) {
		return query(new Compound(ABOLISH_CATEGORY, asList(category)));
	}

	public Query extendsCategory(Term child, Term parent) {
		return query(new Compound(EXTENDS_CATEGORY, asList(child, parent)));
	}
	
	public Query extendsCategory(Term child, Term parent, Term scope) {
		return query(new Compound(EXTENDS_CATEGORY, asList(child, parent, scope)));
	}
	
	public Query categoryProperty(Term category, Term property) {
		return query(new Compound(CATEGORY_PROPERTY, asList(category, property)));
	}
	
	public Query currentEvent(Term event, Term object, Term message, Term sender, Term monitor) {
		return query(new Compound(CURRENT_EVENT, asList(event, object, message, sender, monitor)));
	}
	
	public Query defineEvents(Term event, Term object, Term message, Term sender, Term monitor) {
		return query(new Compound(DEFINE_EVENTS, asList(event, object, message, sender, monitor)));
	}
	
	public Query abolishEvents(Term event, Term object, Term message, Term sender, Term monitor) {
		return query(new Compound(ABOLISH_EVENTS, asList(event, object, message, sender, monitor)));
	}

	public LogtalkEngine asLogtalkEngine() {
		return this;
	}


}
