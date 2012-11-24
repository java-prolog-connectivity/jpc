package org.jpc.engine.flags;



public abstract class LogtalkFlag {
	public static UnknownEntities UNKNOWN = new UnknownEntities();
	public static Portability PORTABILITY = new Portability();
	public static Report REPORT = new Report();
	public static Optimize OPTIMIZE = new Optimize();
	
	static class UnknownEntities extends LogtalkFlag {
		public static final String WARNING = "warning";
		public static final String SILENT = "silent";
		
		@Override
		public String toString() {
			return "unknown_entities";
		}
	} 
	
	
	static class Portability extends LogtalkFlag {
		public static final String WARNING = "warning";
		public static final String SILENT = "silent";
		
		@Override
		public String toString() {
			return "portability";
		}
	} 
	
	static class Report extends LogtalkFlag {
		public static final String ON = "on";
		public static final String OFF = "off";
		public static final String WARNINGS = "warnings";
		
		@Override
		public String toString() {
			return "report";
		}
	} 

	
	static class Optimize extends LogtalkFlag {
		public static final String ON = "on";
		public static final String OFF = "off";
		
		@Override
		public String toString() {
			return "optimize";
		}
	} 

}
