package org.jpc.query;

import java.util.List;

public interface QueryListener {

	void onQueryReady(); //the query is ready to receive any request
	
	void onQueryOpened(); //the query is in the open state (e.g., after a first "next solution" request).
	
	void onQueryExhausted(); //the query was exhausted when attempting to find the requested solutions (e.g., upon a "next solution" request, or a "solution range" request with less solutions than requested)
	
	void onQueryInProgress(); //the Prolog engine is actively working to find a solution
	
	void onQueryFinished(); //the Prolog engine has found a solution
	
	void onException(Exception e); //an exception was thrown while executing a Prolog query 
	
	void onNextSolutionFound(Solution solution); //a single solution was found upon a next solution request
	
	void onSolutionsFound(List<Solution> solutions); //a group of solutions (e.g., all solutions or a solution range) was found

	void onQueryDisposed(); //the query has been explicitly destroyed

}
