package org.jpc.util.salt;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.DefaultHandler;

public class TermToStringBuilder extends DefaultHandler {

	private PrologEngine logicEngine;
	private StringBuilder sb;
	private boolean skipCompoundArgSeparator;
	
	public TermToStringBuilder(PrologEngine logicEngine) {
		this.logicEngine = logicEngine;
		this.sb = new StringBuilder();
	}

	@Override
	public void startIntegerTerm(long value) {
		sb.append(value);
	}

	@Override
	public void startFloatTerm(double value) {
		sb.append(value);
	}

	@Override
	public void startVariable(String name) {
		sb.append(name);
	}

	@Override
	public void startAtom(String name) {
		String escapedName = logicEngine.escape(name);
		sb.append(escapedName);
	}

	@Override
	public void startCompoundArgs() {
		sb.append("(");
		skipCompoundArgSeparator = true;
	}

	@Override
	public void endCompoundArgs() {
		sb.append(")");
	}

	@Override
	public void startCompoundArg() {
		if(skipCompoundArgSeparator)
			skipCompoundArgSeparator = false;
		else
			sb.append(",");
	}

	@Override
	public String toString() {
		return sb.toString();
	}
}
