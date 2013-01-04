package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.query.Query;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.TermConvertable;

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
	
	
	public Logtalk(PrologEngine logicEngine) {
		super(logicEngine, new Atom(LogtalkConstants.LOGTALK_LOGTALK_OBJECT));
	}

	public Query expandLibraryPath(TermConvertable library, TermConvertable path) {
		return perform(new Compound(EXPAND_LIBRARY_PATH, asList(library, path)));
	}

	public Query loadedFile(TermConvertable file, TermConvertable directory) {
		return perform(new Compound(LOADED_FILE, asList(file, directory)));
	}
	
	public Query loadedFile(TermConvertable file, TermConvertable directory, TermConvertable options) {
		return perform(new Compound(LOADED_FILE, asList(file, directory, options)));
	}
	
	public Query loadedFile(TermConvertable file, TermConvertable directory, TermConvertable options, TermConvertable streamProperties) {
		return perform(new Compound(LOADED_FILE, asList(file, directory, options, streamProperties)));
	}
	
	public Query compileAuxClauses(TermConvertable clauses) {
		return perform(new Compound(COMPILE_AUX_CLAUSES, asList(clauses)));
	}
	
	public Query entityPrefix(TermConvertable entity, TermConvertable prefix) {
		return perform(new Compound(ENTITY_PREFIX, asList(entity, prefix)));
	}
	
	public Query compilePredicateHeads(TermConvertable heads, TermConvertable translatedHeads) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, asList(heads, translatedHeads)));
	}
	
	public Query compilePredicateHeads(TermConvertable heads, TermConvertable translatedHeads, TermConvertable contextArgument) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, asList(heads, translatedHeads, contextArgument)));
	}
	
	public Query compilePredicateHeads(TermConvertable heads, TermConvertable entity, TermConvertable translatedHeads, TermConvertable contextArgument) {
		return perform(new Compound(COMPILE_PREDICATE_HEADS, asList(heads, entity, translatedHeads, contextArgument)));
	}
	
	public Query compilePredicateIndicators(TermConvertable predicateIndicators, TermConvertable translatedPredicateIndicators) {
		return perform(new Compound(COMPILE_PREDICATE_INDICATORS, asList(predicateIndicators, translatedPredicateIndicators)));
	}

	public Query compilePredicateIndicators(TermConvertable predicateIndicators, TermConvertable entity, TermConvertable translatedPredicateIndicators) {
		return perform(new Compound(COMPILE_PREDICATE_INDICATORS, asList(predicateIndicators, entity, translatedPredicateIndicators)));
	}
	
	public Query decompilePredicateHeads(TermConvertable translatedHeads, TermConvertable heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, asList(translatedHeads, heads)));
	}
	
	public Query decompilePredicateHeads(TermConvertable translatedHeads, TermConvertable entity, TermConvertable heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, asList(translatedHeads, entity, heads)));
	}
	
	public Query decompilePredicateHeads(TermConvertable translatedHeads, TermConvertable entity, TermConvertable entityType, TermConvertable heads) {
		return perform(new Compound(DECOMPILE_PREDICATE_HEADS, asList(translatedHeads, entity, entityType, heads)));
	}
	
	public Query decompilePredicateIndicators(TermConvertable translatedPredicateIndicators, TermConvertable predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, asList(translatedPredicateIndicators, predicateIndicators)));
	}
	
	public Query decompilePredicateIndicators(TermConvertable translatedPredicateIndicators, TermConvertable entity, TermConvertable predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, asList(translatedPredicateIndicators, entity, predicateIndicators)));
	}
	
	public Query decompilePredicateIndicators(TermConvertable translatedPredicateIndicators, TermConvertable entity, TermConvertable entityType, TermConvertable predicateIndicators) {
		return perform(new Compound(DECOMPILE_PREDICATE_INDICATORS, asList(translatedPredicateIndicators, entity, entityType, predicateIndicators)));
	}
	
	public Query executionContext(TermConvertable executionContext, TermConvertable sender, TermConvertable thiz, TermConvertable self, TermConvertable metacallContext, TermConvertable stack) {
		return perform(new Compound(EXECUTION_CONTEXT, asList(executionContext, sender, thiz, self, metacallContext, stack)));
	}
	
	public Query printMessage(TermConvertable kind, TermConvertable component, TermConvertable term) {
		return perform(new Compound(PRINT_MESSAGE, asList(kind, component, term)));
	}
	
	public Query printMessageTokens(TermConvertable stream, TermConvertable prefix, TermConvertable tokens) {
		return perform(new Compound(PRINT_MESSAGE_TOKENS, asList(stream, prefix, tokens)));
	}
	
	public Query printMessageTokens(TermConvertable stream, TermConvertable tokens) {
		return perform(new Compound(PRINT_MESSAGE_TOKENS, asList(stream, tokens)));
	}
	
	public Query messageTokens(TermConvertable term, TermConvertable component) {
		return perform(new Compound(MESSAGE_TOKENS, asList(term, component)));
	}
	
	public Query messagePrefixStream(TermConvertable kind, TermConvertable component, TermConvertable prefix, TermConvertable stream) {
		return perform(new Compound(MESSAGE_PREFIX_STREAM, asList(kind, component, prefix, stream)));
	}
	
	public Query messageHook(TermConvertable term, TermConvertable kind, TermConvertable component, TermConvertable tokens) {
		return perform(new Compound(MESSAGE_HOOK, asList(term, kind, component, tokens)));
	}
	
	public Query traceEvent(TermConvertable event, TermConvertable eventExecutionContext) {
		return perform(new Compound(TRACE_EVENT, asList(event, eventExecutionContext)));
	}
	
	public Query debugHandlerProvider(TermConvertable provider) {
		return perform(new Compound(DEBUG_HANDLER_PROVIDER, asList(provider)));
	}
	
	public Query debugHandler(TermConvertable event, TermConvertable eventExecutionContext) {
		return perform(new Compound(DEBUG_HANDLER, asList(event, eventExecutionContext)));
	}
	
}
