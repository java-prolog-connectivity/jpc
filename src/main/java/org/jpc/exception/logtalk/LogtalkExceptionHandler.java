package org.jpc.exception.logtalk;

import static java.util.Arrays.asList;
import static org.jpc.term.Variable.ANONYMOUS_VAR;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.exception.ExceptionHandlerManager;
import org.jpc.exception.MatchExceptionHandler;
import org.jpc.exception.logtalk.compiletime.LgtCompileTimeExceptionHandler;
import org.jpc.exception.logtalk.runtime.LgtDomainError;
import org.jpc.exception.logtalk.runtime.LgtEvaluationError;
import org.jpc.exception.logtalk.runtime.LgtExistenceError;
import org.jpc.exception.logtalk.runtime.LgtInstantiationError;
import org.jpc.exception.logtalk.runtime.LgtPermissionError;
import org.jpc.exception.logtalk.runtime.LgtRepresentationError;
import org.jpc.exception.logtalk.runtime.LgtResourceError;
import org.jpc.exception.logtalk.runtime.LgtSyntaxError;
import org.jpc.exception.logtalk.runtime.LgtSystemError;
import org.jpc.exception.logtalk.runtime.LgtTypeError;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class LogtalkExceptionHandler extends ExceptionHandlerManager {

	private static Term runTimeExceptionFromExceptionTerm(Term exceptionTerm) {
		return new Compound("error", asList(exceptionTerm, ANONYMOUS_VAR));
	}
	
	private static Term compileTimeExceptionFromCulprit(Term culprit) {
		return new Compound("error", asList(ANONYMOUS_VAR, culprit));
	}
	
	public LogtalkExceptionHandler() {
		registerCompileTimeExceptionHandlers();
		registerRunTimeExceptionHandlers();
	}
	
	private void registerCompileTimeExceptionHandlers() {
		register(new LgtCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("clause", asList(ANONYMOUS_VAR)))));
		register(new LgtCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("directive", asList(ANONYMOUS_VAR)))));
		register(new LgtCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("term", asList(ANONYMOUS_VAR)))));
	}

	private void registerRunTimeExceptionHandlers() {
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Atom("instantiation_error"))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtInstantiationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Atom("system_error"))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtSystemError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("type_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtTypeError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("domain_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtDomainError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("permission_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtPermissionError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("existence_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtExistenceError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("representation_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtRepresentationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("evaluation_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtEvaluationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("resource_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtResourceError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("syntax_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LgtSyntaxError(unifiedExceptionTerm, goal);
			}
		});
	}
	
}
