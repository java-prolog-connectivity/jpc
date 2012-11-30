package org.jpc.engine;

import java.util.ArrayList;
import static java.util.Arrays.asList;
import java.util.List;
import java.util.Map;

import org.jpc.engine.flags.LogtalkFlag;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.AbstractTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import static org.jpc.util.LogicUtil.*;

/**
 * Extends the BootstrapLogicEngine by composition
 * @author sergioc
 *
 */
public class LogicEngine {

	private BootstrapLogicEngine bootstrapEngine;

	public LogicEngine(BootstrapLogicEngine bootstrapEngine) {
		this.bootstrapEngine = bootstrapEngine;
	}
	
	
	//UTILITY METHODS DEPENDING ON A LOGIC ENGINE
	public Query createQuery(Term term) {
		return bootstrapEngine.createQuery(term);
	}
	
	public Query createQuery(String termString) {
		return createQuery(asTerm(termString));
	}
	
	public Term asTerm(String termString) {
		return asTerm(termString, false);
	}
	
	public Term asTerm(String termString, boolean force) {
		try {
			return bootstrapEngine.asTerm(termString);
		} catch(Exception e) {
			if(force)
				return new Atom(termString);
			else
				throw e;
		}
	}
	
	public List<Term> asTerms(List<String> termsString) {
		return asTerms(termsString, false);
	}
	
	public List<Term> asTerms(List<String> termsString, boolean force) {
		List<Term> terms = new ArrayList<>();
		for(String s : termsString)
			terms.add(asTerm(s, force));
		return terms;
	}

	public List<Term> asResourceTerms(List<String> resourceNames) {
		List<Term> terms = new ArrayList<>();
		for(String s : resourceNames)
			terms.add(asResourceTerm(s));
		return terms;
	}
	
	public Term asResourceTerm(String resourceName) {
		if(isResourceAlias(resourceName)) //it is a resource alias of the form library(lib_name)
			return asTerm(resourceName);
		else
			return new Atom(resourceName);
	}
	
	public boolean assertTerms(List<Term> terms) {
		return bootstrapEngine.assertTerms(terms);
	}

	public boolean ensureLoaded(List<Term> resources) {
		return bootstrapEngine.ensureLoaded(resources);
	}

	public boolean ensureLoaded(String... resources) {
		return ensureLoaded(asResourceTerms(asList(resources)));
	}

	public boolean allSucceed(List<Term> terms) {
		return bootstrapEngine.allSucceed(terms);
	}
	
	
	public boolean hasSolution(Term term) {
		return bootstrapEngine.createQuery(term).hasSolution();
	}
	
	public long numberOfSolutions(Term term) {
		return bootstrapEngine.createQuery(term).numberOfSolutions();
	}
	
	public Map<String, Term> oneSolution(Term term) {
		return bootstrapEngine.createQuery(term).oneSolution();
	}
	
	public List<Map<String, Term>> nSolutions(Term term, long n) {
		return bootstrapEngine.createQuery(term).nSolutions(n);
	}
	
	public synchronized List<Map<String, Term>> solutionsRange(Term term, long from, long to) {
		return bootstrapEngine.createQuery(term).solutionsRange(from, to);
	}
	
	public List<Map<String, Term>> allSolutions(Term term) {
		return bootstrapEngine.createQuery(term).allSolutions();
	}
	
	public boolean hasSolution(String queryString) {
		return bootstrapEngine.createQuery(asTerm(queryString)).hasSolution();
	}
	
	public long numberOfSolutions(String queryString) {
		return bootstrapEngine.createQuery(asTerm(queryString)).numberOfSolutions();
	}
	
	public Map<String, Term> oneSolution(String queryString) {
		return bootstrapEngine.createQuery(asTerm(queryString)).oneSolution();
	}
	
	public List<Map<String, Term>> nSolutions(String queryString, long n) {
		return bootstrapEngine.createQuery(asTerm(queryString)).nSolutions(n);
	}
	
	public synchronized List<Map<String, Term>> solutionsRange(String queryString, long from, long to) {
		return bootstrapEngine.createQuery(asTerm(queryString)).solutionsRange(from, to);
	}
	
	public List<Map<String, Term>> allSolutions(String queryString) {
		return bootstrapEngine.createQuery(asTerm(queryString)).allSolutions();
	}
	
	public String currentPrologFlag(String flagName) {
		return bootstrapEngine.currentPrologFlag(flagName);
	}
	
	public String prologDialect() {
		return bootstrapEngine.prologDialect();
	}
	
	public boolean cd(String path) {
		Compound compound = new Compound("cd", asList(new Atom(path)));
		return hasSolution(compound);
	}

	
	/**
	 * Answers an array of anonymous logic variables
	 * @param n the number of variables in the array
	 * @return
	 */
	public static List<Variable> anonymousVariables(int n) {
		List<Variable> variablesList = new ArrayList<>();
		for(int i=0; i<n; i++) {
			variablesList.add(Variable.ANONYMOUS_VAR);
			//variablesList.add(engine.getAnonymousVariable());
		}
		return variablesList;
	}
	


	public boolean isBinaryOperator(String op) {
		return hasSolution("Op='" + op + "', current_op(_, Type, Op), atom_chars(Type, Chars), Chars=[_, f, _]");
	}
	
	public boolean isUnaryOperator(String op) {
		return hasSolution("Op='" + op + "', current_op(_, Type, Op), atom_chars(Type, Chars), Chars=[f, _]");
	}
	
	public boolean flushOutput() {
		return bootstrapEngine.flushOutput();
	}
	
	public Term termsToSequence(List<Term> terms) {
		String sequenceString = termsToTextSequence(terms);
		return asTerm(sequenceString);
	}
	
	public String termsToTextSequence(List<Term> terms) {
		String sequenceString = "";
		for(int i = 0; i<terms.size(); i++) {
			sequenceString += terms.get(i).toString();
			if(i<terms.size()-1)
				sequenceString += ", ";
		}
		return sequenceString;
	}
	
	public List<Term> sequenceAsTerms(Term termSequence) {
		int len = sequenceLength(termSequence);
		Term[] ts = new Term[len];
		for (int i = 0; i < len; i++) {
			if(i<len-1) {
				ts[i] = termSequence.arg(1);
				termSequence = termSequence.arg(2);
			} else
				ts[i] = termSequence;
		}
		return asList(ts);
	} 

	public int sequenceLength(Term sequence) {
		int length = 1;
		if(sequence instanceof Compound) {
			if(sequence.hasFunctor(",", 2))
				length = 1 + sequenceLength(sequence.arg(2));
		}
		return length;
	}
	
	
	
	
	//LOGTALK methods


	public boolean logtalkLoad(String... resources) {
		return logtalkLoad(asResourceTerms(asList(resources)));
	}
	
	public boolean logtalkLoad(List<Term> resourceTerms) {
		return allSucceed(forAllApplyFunctor("logtalk_load", resourceTerms));
	}
	
	public boolean setLogtalkFlag(LogtalkFlag flag, String value) {
		return hasSolution(new Compound("set_logtalk_flag", asList(new Atom(flag.toString()), new Atom(value))));
	}
	
	public List<Term> currentLogtalkObjects() {
		List<Term> currentObjects = new ArrayList<>();
		Variable logtalkObjectVar = new Variable("LogtalkObject");
		Compound compound = new Compound("current_object", asList(logtalkObjectVar));
		for(Map<String, Term> solution : allSolutions(compound)) {
			currentObjects.add(solution.get(logtalkObjectVar.name()));
		}
		return currentObjects;
	}
	
	/**
	 * 
	 * @param object
	 * @return a list of arities of all the Logtalk objects in the logic side having as name the parameter of the Java method
	 */
	//currently assuming that the cardinalities of the objects in the logtalk side are returned ordered from the lowest to the highest
	public List<Integer> numberParametersLogtalkObject(String object) {
		List<Term> currentObjects = currentLogtalkObjects();
		List<Integer> numberParams = new ArrayList<>();
		for(Term currentObject: currentObjects) {
			if(currentObject instanceof Atom) {
				Atom atom = (Atom)currentObject;
				if(atom.name().equals(object))
					numberParams.add(0);
			} else if(currentObject instanceof Compound) {
				Compound compound = (Compound)currentObject;
				if(compound.name().equals(object))
					numberParams.add(compound.arity());
			}
		}
		return numberParams;
	}
	
	/*
	public static boolean usePrologNativeModule(String moduleName) {
		//e.g., lists, charsio
		 Query useModule = new Query(surround(surround(moduleName, "library"), "use_module"));
		 return useModule.hasSolution();
	}
	*/

/*
	public String[] solutionsForVars(Hashtable[] solutions, Variable var) {
		String[][] allVarSolutionsAux = solutionsForVars(solutions, new Variable[] {var});
		String[] allVarSolutions = new String[allVarSolutionsAux.length];
		
		for(int i=0; i<allVarSolutions.length; i++) {
			allVarSolutions[i] = allVarSolutionsAux[i][0];
		}
		return allVarSolutions;
	}
	

	public String[][] solutionsForVars(Hashtable[] solutions, Variable[] vars) {
		int numberOfSolutions = solutions.length;
		int numberOfVars = vars.length;
		String[][] solutionsTable = new String[numberOfSolutions][numberOfVars];
		
		for(int i = 0; i<numberOfSolutions; i++) 
			for(int j = 0; j<numberOfVars; j++)
				solutionsTable[i][j] = solutionForVar(solutions[i], vars[j]);
		
		return solutionsTable;
	}
	
	protected String solutionForVar(Hashtable solution, Variable var) {
		return solution.get(var.toString()).toString();
	}
	
	*/
	
}
