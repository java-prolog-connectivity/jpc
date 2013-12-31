package org.jpc.term.compiled;

public class QueryVar extends EnvironmentVar {

	public static final int QUERY_CODE = -1;
	
	private final String name; 
	
	public QueryVar(String name, int varId) {
		super(QUERY_CODE, varId, QUERY_CODE);
		this.name = name;
	}
	
	@Override
	public String getName() {
		return name;
	}
	
}
