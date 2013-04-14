package org.jpc.engine.logtalk;

import org.jpc.engine.prolog.Flag;


/**
 * Some Logtalk flags
 * (to be completed)
 * @author sergioc
 *
 */
public abstract class LogtalkFlag extends Flag {
	
	public static UnknownEntities UNKNOWN_ENTITIES = new UnknownEntities();
	public static Portability PORTABILITY = new Portability();
	public static UnderscoreVariables UNDERSCORE_VARIABLES = new UnderscoreVariables();
	public static Complements COMPLEMENTS = new Complements();
	public static DynamicDeclarations DYNAMIC_DECLARATIONS = new DynamicDeclarations();
	public static Events EVENTS = new Events();
	public static ContextSwitchingCalls CONTEXT_SWITCHING_CALLS = new ContextSwitchingCalls();
	public static Report REPORT = new Report();
	public static Optimize OPTIMIZE = new Optimize();
	public static Hook HOOK = new Hook();
	public static Clean CLEAN = new Clean();
	
	public LogtalkFlag(String name) {
		super(name);
	}
	
	//Lint flags
	
	static class UnknownEntities extends LogtalkFlag {
		public static final String WARNING = "warning";
		public static final String SILENT = "silent";
		
		public UnknownEntities() {super("unknown_entities");}
	} 
	
	
	static class Portability extends LogtalkFlag {
		public static final String WARNING = "warning";
		public static final String SILENT = "silent";
		
		public Portability() {super("portability");}
	} 
	
	
	static class UnderscoreVariables extends LogtalkFlag {
		public static final String DONT_CARE = "dont_care";
		public static final String SINGLETONS = "singletons";
		
		public UnderscoreVariables() {super("underscore_variables");}
	} 
	
	
	//Optional features compilation flags
	
	static class Complements extends LogtalkFlag {
		public static final String ALLOW = "allow";
		public static final String DENY = "deny";
		
		public Complements() {super("complements");}
	} 
	
	
	static class DynamicDeclarations extends LogtalkFlag {
		public static final String ALLOW = "allow";
		public static final String DENY = "deny";
		
		public DynamicDeclarations() {super("dynamic_declarations");}
	} 
	
	
	static class Events extends LogtalkFlag {
		public static final String ALLOW = "allow";
		public static final String DENY = "deny";
		
		public Events() {super("events");}
	} 
	
	
	static class ContextSwitchingCalls extends LogtalkFlag {
		public static final String ALLOW = "allow";
		public static final String DENY = "deny";
		
		public ContextSwitchingCalls() {super("context_switching_calls");}
	} 
	
	//Other flags
	
	static class Report extends LogtalkFlag {
		public static final String ON = "on";
		public static final String OFF = "off";
		public static final String WARNINGS = "warnings";
		
		public Report() {super("report");}
	} 

	
	static class Optimize extends LogtalkFlag {
		public static final String ON = "on";
		public static final String OFF = "off";
		
		public Optimize() {super("optimize");}
	} 
	
	
	static class Hook extends LogtalkFlag {
		//no default values, this option specifies an object
		public Hook() {super("hook");}
	} 
	
	
	static class Clean extends LogtalkFlag {
		public static final String ON = "on";
		public static final String OFF = "off";
		
		public Clean() {super("clean");}
	} 

}
