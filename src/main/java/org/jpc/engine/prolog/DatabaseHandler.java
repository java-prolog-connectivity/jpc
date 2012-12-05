package org.jpc.engine.prolog;

import org.jpc.term.TermConvertable;

public interface DatabaseHandler {

	public abstract boolean asserta(TermConvertable termConvertable);
	
	public boolean assertz(TermConvertable termConvertable);
	
	public Query retract(TermConvertable termConvertable);
	
	public boolean retractAll(TermConvertable termConvertable);
	
	public boolean abolish(TermConvertable termConvertable);
	
	public Query clause(TermConvertable head, TermConvertable body);

}
