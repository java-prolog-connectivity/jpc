package org.jpc.exception.logtalk;

import static java.util.Arrays.asList;
import static org.jpc.term.Variable.ANONYMOUS_VAR;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.exception.ExceptionHandlerManager;
import org.jpc.exception.MatchExceptionHandler;
import org.jpc.exception.logtalk.compiletime.LogtalkCompileTimeExceptionHandler;
import org.jpc.exception.logtalk.runtime.LogtalkDomainError;
import org.jpc.exception.logtalk.runtime.LogtalkEvaluationError;
import org.jpc.exception.logtalk.runtime.LogtalkExistenceError;
import org.jpc.exception.logtalk.runtime.LogtalkInstantiationError;
import org.jpc.exception.logtalk.runtime.LogtalkPermissionError;
import org.jpc.exception.logtalk.runtime.LogtalkRepresentationError;
import org.jpc.exception.logtalk.runtime.LogtalkResourceError;
import org.jpc.exception.logtalk.runtime.LogtalkSyntaxError;
import org.jpc.exception.logtalk.runtime.LogtalkSystemError;
import org.jpc.exception.logtalk.runtime.LogtalkTypeError;
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
		register(new LogtalkCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("clause", asList(ANONYMOUS_VAR)))));
		register(new LogtalkCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("directive", asList(ANONYMOUS_VAR)))));
		register(new LogtalkCompileTimeExceptionHandler(compileTimeExceptionFromCulprit(new Compound("term", asList(ANONYMOUS_VAR)))));
	}

	private void registerRunTimeExceptionHandlers() {
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Atom("instantiation_error"))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkInstantiationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Atom("system_error"))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkSystemError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("type_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkTypeError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("domain_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkDomainError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("permission_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkPermissionError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("existence_error", asList(ANONYMOUS_VAR,ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkExistenceError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("representation_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkRepresentationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("evaluation_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkEvaluationError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("resource_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkResourceError(unifiedExceptionTerm, goal);
			}
		});
		
		register(new MatchExceptionHandler(runTimeExceptionFromExceptionTerm(new Compound("syntax_error", asList(ANONYMOUS_VAR)))) {
			@Override
			public void onMatch(PrologEngine prologEngine, Term unifiedExceptionTerm, Term goal) {
				throw new LogtalkSyntaxError(unifiedExceptionTerm, goal);
			}
		});
	}
	
}
