package org.jpc.query;

import java.util.List;
import java.util.Map;

import org.jpc.term.Term;

public interface QueryListener {

	public void onQueryReady();
	
	public void onQueryOpened();
	
	public void onQueryExhausted();
	
	public void onNextSolution(Map<String,Term> solution);
	
	public void onAllSolutions(List<Map<String,Term>> solutions);

}
