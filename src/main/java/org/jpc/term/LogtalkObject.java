package org.jpc.term;

import static java.util.Arrays.asList;

public class LogtalkObject implements TermConvertable {

	public static final String LOGTALK_OPERATOR = "::";
	
	private TermConvertable termConvertable;
	
	public LogtalkObject(TermConvertable termConvertable) {
		this.termConvertable = termConvertable;
	}
	
	public Term sendMessage(TermConvertable message) {
		return new Compound("::", asList(this, message));
	}
	
	@Override
	public Term asTerm() {
		return termConvertable.asTerm();
	}
}
