package org.jpc.engine.logtalk;

import java.util.Arrays;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Query;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;

/**
 * The built-in object 'logtalk'
 * @author sergioc
 *
 */
public class Logtalk extends LogtalkObject {

	public static final String EXPAND_LIBRARY_PATH = "expand_library_path";
	
	public static final String LOADED_FILE = "loaded_file";
	
	public static final String COMPILE_AUX_CLAUSES = "compile_aux_clauses";
	
	public static final String ENTITY_PREFIX = "entity_prefix";
	
	public static final String COMPILE_PREDICATE_HEADS = "compile_predicate_heads";
	
	public static final String COMPILE_PREDICATE_INDICATORS = "compile_predicate_indicators";
	
	public static final String DECOMPILE_PREDICATE_HEADS = "decompile_predicate_heads";
	
	public static final String DECOMPILE_PREDICATE_INDICATORS = "decompile_predicate_indicators";
	
	public static final String EXECUTION_CONTEXT = "execution_context";
	
	public static final String PRINT_MESSAGE = "print_message";
	
	public static final String PRINT_MESSAGE_TOKENS = "print_message_tokens";
	
	public static final String MESSAGE_TOKENS = "message_tokens";
	
	public static final String MESSAGE_PREFIX_STREAM = "message_prefix_stream";
	
	public static final String MESSAGE_HOOK = "message_hook";
	
	public static final String TRACE_EVENT = "trace_event";
	
	public static final String DEBUG_HANDLER_PROVIDER = "debug_handler_provider";
	
	public static final String DEBUG_HANDLER = "debug_handler";
	
	
	public Logtalk(PrologEngine prologEngine) {
		super(new Atom(LogtalkConstants.LOGTALK_LOGTALK_OBJECT), prologEngine);
	}

	public Query expandLibraryPath(Term library, Term path) {
		return perform(new Compound(EXPAND_LIBRARY_PATH, Arrays.asList(library, path)));
	}

	public Query loadedFile(Term file, Term directory) {
		return perform(new Compound(LOADED_FILE, Arrays.asList(file, directory)));
	}
	
	public Query loadedFile(Term file, Term directory, Term options) {
		return perform(new Compound(LOADED_FILE, Arrays.asList(file, directory, options)));
	}
	
	public Query loadedFile(Term file, Term directory, Term options, Term streamProperties) {
		return perform(new Compound(LOADED_FILE, Arrays.asList(file, directory, options, streamProperties)));
	}
	
	public Query compileAuxClauses(Term clauses) {
		return perform(new Compound(COMPILE_AUX_CLAUSES, Arrays.asList(clauses)));
	}
	
	public Query entityPrefix(Term entity, Term prefix) {
		return perform(new Compound(ENTITY_PREFIX, Arrays.asList(entity, prefix)));
	}
	
	public Query compilePredicateHeads(Term heads, Term translatedHeads) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, Arrays.asList(heads, translatedHeads)));
	}
	
	public Query compilePredicateHeads(Term heads, Term translatedHeads, Term contextArgument) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, Arrays.asList(heads, translatedHeads, contextArgument)));
	}
	
	public Query compilePredicateHeads(Term heads, Term entity, Term translatedHeads, Term contextArgument) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, Arrays.asList(heads, entity, translatedHeads, contextArgument)));
	}
	
	public Query compilePredicateIndicators(Term predicateIndicators, Term translatedPredicateIndicators) {
		return perform(new Compound(COMPILE_PREDICATE_INDICATORS, Arrays.asList(predicateIndicators, translatedPredicateIndicators)));
	}

	public Query compilePredicateIndicators(Term predicateIndicators, Term entity, Term translatedPredicateIndicators) {
		return perform(new Compound(COMPILE_PREDICATE_INDICATORS, Arrays.asList(predicateIndicators, entity, translatedPredicateIndicators)));
	}
	
	public Query decompilePredicateHeads(Term translatedHeads, Term heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, Arrays.asList(translatedHeads, heads)));
	}
	
	public Query decompilePredicateHeads(Term translatedHeads, Term entity, Term heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, Arrays.asList(translatedHeads, entity, heads)));
	}
	
	public Query decompilePredicateHeads(Term translatedHeads, Term entity, Term entityType, Term heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, Arrays.asList(translatedHeads, entity, entityType, heads)));
	}
	
	public Query decompilePredicateIndicators(Term translatedPredicateIndicators, Term predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, Arrays.asList(translatedPredicateIndicators, predicateIndicators)));
	}
	
	public Query decompilePredicateIndicators(Term translatedPredicateIndicators, Term entity, Term predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, Arrays.asList(translatedPredicateIndicators, entity, predicateIndicators)));
	}
	
	public Query decompilePredicateIndicators(Term translatedPredicateIndicators, Term entity, Term entityType, Term predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, Arrays.asList(translatedPredicateIndicators, entity, entityType, predicateIndicators)));
	}
	
	public Query executionContext(Term executionContext, Term sender, Term thiz, Term self, Term metacallContext, Term stack) {
		return perform(new Compound(EXECUTION_CONTEXT, Arrays.asList(executionContext, sender, thiz, self, metacallContext, stack)));
	}
	
	public Query printMessage(Term kind, Term component, Term term) {
		return perform(new Compound(PRINT_MESSAGE, Arrays.asList(kind, component, term)));
	}
	
	public Query printMessageTokens(Term stream, Term prefix, Term tokens) {
		return perform(new Compound(PRINT_MESSAGE_TOKENS, Arrays.asList(stream, prefix, tokens)));
	}
	
	public Query printMessageTokens(Term stream, Term tokens) {
		return perform(new Compound(PRINT_MESSAGE_TOKENS, Arrays.asList(stream, tokens)));
	}
	
	public Query messageTokens(Term term, Term component) {
		return perform(new Compound(MESSAGE_TOKENS, Arrays.asList(term, component)));
	}
	
	public Query messagePrefixStream(Term kind, Term component, Term prefix, Term stream) {
		return perform(new Compound(MESSAGE_PREFIX_STREAM, Arrays.asList(kind, component, prefix, stream)));
	}
	
	public Query messageHook(Term term, Term kind, Term component, Term tokens) {
		return perform(new Compound(MESSAGE_HOOK, Arrays.asList(term, kind, component, tokens)));
	}
	
	public Query traceEvent(Term event, Term eventExecutionContext) {
		return perform(new Compound(TRACE_EVENT, Arrays.asList(event, eventExecutionContext)));
	}
	
	public Query debugHandlerProvider(Term provider) {
		return perform(new Compound(DEBUG_HANDLER_PROVIDER, Arrays.asList(provider)));
	}
	
	public Query debugHandler(Term event, Term eventExecutionContext) {
		return perform(new Compound(DEBUG_HANDLER, Arrays.asList(event, eventExecutionContext)));
	}
	
}
