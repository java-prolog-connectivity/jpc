package org.jpc.converter.typesolver;

import static java.util.Arrays.asList;
import static org.jpc.term.Functor.functor;

import java.lang.reflect.Type;

import org.jgum.JGum;
import org.jpc.JpcException;
import org.jpc.converter.typesolver.catalog.AtomTypeSolver;
import org.jpc.converter.typesolver.catalog.CharacterTypeSolver;
import org.jpc.converter.typesolver.catalog.FloatTermTypeSolver;
import org.jpc.converter.typesolver.catalog.IntegerTermTypeSolver;
import org.jpc.converter.typesolver.catalog.ListTypeSolver;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.converter.typesolver.catalog.NumberTypeSolver;
import org.jpc.converter.typesolver.catalog.StringTypeSolver;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.JpcPreferences;

public class JpcTypeSolverManager extends TypeSolverManagerImpl {
	
	private static final String TYPE_SOLVER_FUNCTOR_NAME = "type_solver";
	
	/**
	 * Registers default type solvers in the given type solver manager.
	 * @param typeSolverManager a type solver manager.
	 */
	public static JpcTypeSolverManager registerDefaults(JpcTypeSolverManager typeSolverManager) {
		typeSolverManager.register(new AtomTypeSolver());
		typeSolverManager.register(new IntegerTermTypeSolver());
		typeSolverManager.register(new FloatTermTypeSolver());
		typeSolverManager.register(new ListTypeSolver());
		typeSolverManager.register(new MapTypeSolver());
		typeSolverManager.register(new StringTypeSolver());
		typeSolverManager.register(new NumberTypeSolver());
		typeSolverManager.register(new CharacterTypeSolver());
		
		return typeSolverManager;
	}
	
	private static boolean isValidTypeSolvableTerm(Term term) {
		return term instanceof Compound;// || term instanceof Atom;
	}
	
	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	
	public JpcTypeSolverManager(JGum jgum) {
		this(jgum, new JpcEngine());
	}
	
	public JpcTypeSolverManager(JGum jgum, JpcEngine embeddedEngine) {
		super(jgum);
		this.embeddedEngine = embeddedEngine;
		MutableIndexManager indexManager = embeddedEngine.getIndexManager();
		Functor typeSolverFunctor = functor(TYPE_SOLVER_FUNCTOR_NAME, 2);
		IndexDescriptor indexDescriptor = IndexDescriptor.forIndexedArgument(1, indexManager); //makes use of any index defined for the first argument.
		indexManager.setIndexDescriptor(typeSolverFunctor, indexDescriptor); //clause heads having TYPE_SOLVER_FUNCTOR_NAME as a functor name will be indexed according to the first argument of the term head.
	}

	@Override
	public Type inferType(Object key, Object object) {
		if(object instanceof Term) {
			try {
				return evalQuantifiedTermTypeSolver((Term)object); //the current implementation does not take into consideration the key for finding type solvers in the embedded Prolog database.
			} catch(UnrecognizedObjectException e) {}
		}
		return super.inferType(key, object);
	}
	
	public void register(TypeSolver<?>  typeSolver, Term term) {
		if(!isValidTypeSolvableTerm(term))
			throw new JpcException("Term " + term + " cannot be associated with a type solver.");
		//the current implementation does not take into consideration the key when storing the type solver in the Prolog database.
		//an alternative implementation could add the key as another argument to the predicate with functor name TYPE_SOLVER_FUNCTOR_NAME.
		embeddedEngine.assertz(new Compound(TYPE_SOLVER_FUNCTOR_NAME, asList(term, JRef.jRef(typeSolver))));
	}
	
	/**
	 * Infers the best type for a Prolog term looking for type solvers in the embedded Prolog database.
	 * @param term the term to infer its type.
	 * @return the suggested type for the given term.
	 * @throws UnrecognizedObjectException if no type solver can be found in the embedded database or if no type solver can infer a type for the given term.
	 */
	private Type evalQuantifiedTermTypeSolver(Term term) {
		Type type = null;
		if(isValidTypeSolvableTerm(term)) {
			String typeSolverVarName = JpcPreferences.JPC_VAR_PREFIX + "TypeSolver";
			Query query = embeddedEngine.query(new Compound(TYPE_SOLVER_FUNCTOR_NAME, asList(term, new Var(typeSolverVarName))));
			while(query.hasNext()) {
				Solution solution = query.next();
				Term unifiedTerm = term.replaceVariables(solution);
				TypeSolver typeSolver = (TypeSolver)((JRef)solution.get(typeSolverVarName)).getReferent();
				try {
					type = typeSolver.inferType(term);
				} catch(UnrecognizedObjectException e) {} //just try with the next converter.
			}
			query.close();
		}
		if(type == null)
			throw new UnrecognizedObjectException();
		else
			return type;
	}
	
}
