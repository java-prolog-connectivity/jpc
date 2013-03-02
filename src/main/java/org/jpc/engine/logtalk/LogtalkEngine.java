package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;
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
import static org.jpc.util.LogicUtil.forEachApplyFunctor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Query;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

public class LogtalkEngine implements PrologEngine {
	
	private PrologEngine prologEngine;
	
	public Query query(TermConvertable termConvertable) {
		return prologEngine.query(termConvertable);
	}

	public LogtalkEngine(PrologEngine prologEngine) {
		this.prologEngine = prologEngine;
	}

	public boolean logtalkLoad(String... resources) {
		return logtalkLoad(asResourceTerms(asList(resources)));
	}
	
	public boolean logtalkLoad(List<? extends TermConvertable> termConvertables) {
		return allSucceed(forEachApplyFunctor(LOGTALK_LOAD, termConvertables));
	}
	
	public String currentLogtalkFlag(LogtalkFlag flag) {
		String flagValue = null;
		Variable varFlag = new Variable("Var");
		Map<String, Term> solutions = query(new Compound(CURRENT_LOGTALK_FLAG, Arrays.asList(flag, varFlag))).oneSolution();
		if(solutions!=null) {
			Atom flagValueTerm = (Atom) solutions.get(varFlag.name());
			flagValue = flagValueTerm.getName();
		}
		return flagValue;
	}
	
	public boolean setLogtalkFlag(LogtalkFlag flag, String value) {
		return query(new Compound(SET_LOGTALK_FLAG, asList(new Atom(flag.toString()), new Atom(value)))).hasSolution();
	}
	
	public Query currentObject(TermConvertable term) {
		return query(new Compound(CURRENT_OBJECT, asList(term)));
	}
	
	public ListTerm<LogtalkObject> currentObjects() {
		ListTerm<LogtalkObject> currentObjects = new ListTerm<>();
		Variable logtalkObjectVar = new Variable("LogtalkObject");
		Compound compound = new Compound(CURRENT_OBJECT, asList(logtalkObjectVar));
		for(Map<String, Term> solution : query(compound).allSolutions()) {
			Term currentObjectTerm = solution.get(logtalkObjectVar.name());
			currentObjects.add(new LogtalkObject(currentObjectTerm, this));
		}
		return currentObjects;
	}
	
	/**
	 * 
	 * @param objectName
	 * @return a list of arities of all the Logtalk objects in the logic side having as name the parameter of the Java method
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

	public Query createObject(TermConvertable object, TermConvertable relations, TermConvertable directives, TermConvertable Clauses) {
		return query(new Compound(CREATE_OBJECT, asList(object, relations, directives, Clauses)));
	}
	
	public Query abolishObject(TermConvertable object) {
		return query(new Compound(ABOLISH_OBJECT, asList(object)));
	}
	
	public Query instantiatesClass(TermConvertable instance, TermConvertable clazz) {
		return query(new Compound(INSTANTIATES_CLASS, asList(instance, clazz)));
	}
	
	public Query instantiatesClass(TermConvertable instance, TermConvertable clazz, TermConvertable scope) {
		return query(new Compound(INSTANTIATES_CLASS, asList(instance, clazz, scope)));
	}
	
	public Query specializesClass(TermConvertable clazz, TermConvertable superClass) {
		return query(new Compound(SPECIALIZES_CLASS, asList(clazz, superClass)));
	}
	
	public Query specializesClass(TermConvertable clazz, TermConvertable superClass, TermConvertable scope) {
		return query(new Compound(SPECIALIZES_CLASS, asList(clazz, superClass, scope)));
	}
	
	public Query extendsObject(TermConvertable child, TermConvertable parent) {
		return query(new Compound(EXTENDS_OBJECTS, asList(child, parent)));
	}
	
	public Query extendsObject(TermConvertable child, TermConvertable parent, TermConvertable scope) {
		return query(new Compound(EXTENDS_OBJECTS, asList(child, parent, scope)));
	}
	
	public Query importsCategory(TermConvertable object, TermConvertable category) {
		return query(new Compound(IMPORTS_CATEGORY, asList(object, category)));
	}
	
	public Query importsCategory(TermConvertable object, TermConvertable category, TermConvertable scope) {
		return query(new Compound(IMPORTS_CATEGORY, asList(object, category, scope)));
	}
	
	public Query implementsProtocol(TermConvertable object, TermConvertable protocol) {
		return query(new Compound(IMPLEMENTS_PROTOCOL, asList(object, protocol)));
	}
	
	public Query implementsProtocol(TermConvertable object, TermConvertable protocol, TermConvertable scope) {
		return query(new Compound(IMPLEMENTS_PROTOCOL, asList(object, protocol, scope)));
	}
	
	public Query conformsToProtocol(TermConvertable object, TermConvertable protocol) {
		return query(new Compound(CONFORMS_TO_PROTOCOL, asList(object, protocol)));
	}
	
	public Query conformsToProtocol(TermConvertable object, TermConvertable protocol, TermConvertable scope) {
		return query(new Compound(CONFORMS_TO_PROTOCOL, asList(object, protocol, scope)));
	}
	
	public Query complementsObject(TermConvertable category, TermConvertable object) {
		return query(new Compound(COMPLEMENTS_OBJECT, asList(category, object)));
	}
	
	public Query objectProperty(TermConvertable object, TermConvertable property) {
		return query(new Compound(OBJECT_PROPERTY, asList(object, property)));
	}
	
	
	public Query currentProtocol(TermConvertable protocol) {
		return query(new Compound(CURRENT_PROTOCOL, asList(protocol)));
	}
	
	public Query createProtocol(TermConvertable protocol, TermConvertable relations, TermConvertable directives) {
		return query(new Compound(CREATE_PROTOCOL, asList(protocol, relations, directives)));
	}
	
	public Query abolishProtocol(TermConvertable protocol) {
		return query(new Compound(ABOLISH_PROTOCOL, asList(protocol)));
	}
	
	public Query extendsProtocol(TermConvertable child, TermConvertable parent) {
		return query(new Compound(EXTENDS_PROTOCOL, asList(child, parent)));
	}
	
	public Query extendsProtocol(TermConvertable child, TermConvertable parent, TermConvertable scope) {
		return query(new Compound(EXTENDS_PROTOCOL, asList(child, parent, scope)));
	}
	
	public Query protocolProperty(TermConvertable protocol, TermConvertable property) {
		return query(new Compound(PROTOCOL_PROPERTY, asList(protocol, property)));
	}
	
	
	public Query currentCategory(TermConvertable category) {
		return query(new Compound(CURRENT_CATEGORY, asList(category)));
	}
	
	public Query createCategory(TermConvertable category, TermConvertable relations, TermConvertable directives, TermConvertable Clauses) {
		return query(new Compound(CREATE_CATEGORY, asList(category, relations, directives, Clauses)));
	}
	
	public Query abolishCategory(TermConvertable category) {
		return query(new Compound(ABOLISH_CATEGORY, asList(category)));
	}

	public Query extendsCategory(TermConvertable child, TermConvertable parent) {
		return query(new Compound(EXTENDS_CATEGORY, asList(child, parent)));
	}
	
	public Query extendsCategory(TermConvertable child, TermConvertable parent, TermConvertable scope) {
		return query(new Compound(EXTENDS_CATEGORY, asList(child, parent, scope)));
	}
	
	public Query categoryProperty(TermConvertable category, TermConvertable property) {
		return query(new Compound(CATEGORY_PROPERTY, asList(category, property)));
	}
	
	public Query currentEvent(TermConvertable event, TermConvertable object, TermConvertable message, TermConvertable sender, TermConvertable monitor) {
		return query(new Compound(CURRENT_EVENT, asList(event, object, message, sender, monitor)));
	}
	
	public Query defineEvents(TermConvertable event, TermConvertable object, TermConvertable message, TermConvertable sender, TermConvertable monitor) {
		return query(new Compound(DEFINE_EVENTS, asList(event, object, message, sender, monitor)));
	}
	
	public Query abolishEvents(TermConvertable event, TermConvertable object, TermConvertable message, TermConvertable sender, TermConvertable monitor) {
		return query(new Compound(ABOLISH_EVENTS, asList(event, object, message, sender, monitor)));
	}
	
	
	/* ********************************************************************************************************************************
	 * PROXY METHODS IMPLEMENTED IN LogtalkEngine
     **********************************************************************************************************************************
     */
	
	public LogtalkEngine asLogtalkEngine() {
		return this;
	}

	public boolean stop() {
		return prologEngine.stop();
	}

	public Query query(String termString) {
		return prologEngine.query(termString);
	}

	public Query query(TermConvertable... termConvertables) {
		return prologEngine.query(termConvertables);
	}

	public Query query(List<? extends TermConvertable> termConvertables) {
		return prologEngine.query(termConvertables);
	}

	public Term asTerm(String termString) {
		return prologEngine.asTerm(termString);
	}

	public Term asTerm(String termString, boolean force) {
		return prologEngine.asTerm(termString, force);
	}

	public List<Term> asTerms(List<String> termsString) {
		return prologEngine.asTerms(termsString);
	}

	public List<Term> asTerms(List<String> termsString, boolean force) {
		return prologEngine.asTerms(termsString, force);
	}

	public String escape(String s) {
		return prologEngine.escape(s);
	}

	public boolean setPrologFlag(TermConvertable flag, TermConvertable flagValue) {
		return prologEngine.setPrologFlag(flag, flagValue);
	}

	public boolean setPrologFlag(TermConvertable flag, String flagValue) {
		return prologEngine.setPrologFlag(flag, flagValue);
	}

	public Query currentPrologFlag(TermConvertable flag,
			TermConvertable flagValue) {
		return prologEngine.currentPrologFlag(flag, flagValue);
	}

	public String currentPrologFlag(TermConvertable flag) {
		return prologEngine.currentPrologFlag(flag);
	}

	public String prologDialect() {
		return prologEngine.prologDialect();
	}

	public Query currentOp(TermConvertable priority, TermConvertable specifier,
			TermConvertable operator) {
		return prologEngine.currentOp(priority, specifier, operator);
	}

	public boolean isBinaryOperator(String op) {
		return prologEngine.isBinaryOperator(op);
	}

	public boolean isUnaryOperator(String op) {
		return prologEngine.isUnaryOperator(op);
	}

	public boolean cd(TermConvertable path) {
		return prologEngine.cd(path);
	}

	public boolean cd(String path) {
		return prologEngine.cd(path);
	}

	public boolean asserta(TermConvertable termConvertable) {
		return prologEngine.asserta(termConvertable);
	}

	public boolean assertz(TermConvertable termConvertable) {
		return prologEngine.assertz(termConvertable);
	}

	public Query retract(TermConvertable termConvertable) {
		return prologEngine.retract(termConvertable);
	}

	public boolean retractAll(TermConvertable termConvertable) {
		return prologEngine.retractAll(termConvertable);
	}

	public boolean abolish(TermConvertable termConvertable) {
		return prologEngine.abolish(termConvertable);
	}

	public Query clause(TermConvertable head, TermConvertable body) {
		return prologEngine.clause(head, body);
	}

	public boolean asserta(List<? extends TermConvertable> termConvertables) {
		return prologEngine.asserta(termConvertables);
	}

	public boolean assertz(List<? extends TermConvertable> termConvertables) {
		return prologEngine.assertz(termConvertables);
	}

	public boolean ensureLoaded(List<? extends TermConvertable> termConvertables) {
		return prologEngine.ensureLoaded(termConvertables);
	}

	public boolean ensureLoaded(TermConvertable... termConvertables) {
		return prologEngine.ensureLoaded(termConvertables);
	}

	public boolean ensureLoaded(String... resources) {
		return prologEngine.ensureLoaded(resources);
	}

	public Query bagof(TermConvertable select, TermConvertable exp,
			TermConvertable all) {
		return prologEngine.bagof(select, exp, all);
	}

	public Query findall(TermConvertable select, TermConvertable exp,
			TermConvertable all) {
		return prologEngine.findall(select, exp, all);
	}

	public Query setof(TermConvertable select, TermConvertable exp,
			TermConvertable all) {
		return prologEngine.setof(select, exp, all);
	}

	public Query forall(TermConvertable generator, TermConvertable test) {
		return prologEngine.forall(generator, test);
	}

	public boolean flushOutput() {
		return prologEngine.flushOutput();
	}

	public Term unify(TermConvertable... terms) {
		return prologEngine.unify(terms);
	}
	
	public Term unify(List<? extends TermConvertable> terms) {
		return prologEngine.unify(terms);
	}

	public boolean allSucceed(List<? extends TermConvertable> termConvertables) {
		return prologEngine.allSucceed(termConvertables);
	}

	public List<Term> asResourceTerms(List<String> resourceNames) {
		return prologEngine.asResourceTerms(resourceNames);
	}

	public Term asResourceTerm(String resourceName) {
		return prologEngine.asResourceTerm(resourceName);
	}

	public String termSequenceToString(TermConvertable sequenceTermConvertable) {
		return prologEngine.termSequenceToString(sequenceTermConvertable);
	}

}
