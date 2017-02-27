package org.jpc.util.termprocessor;

import java.io.Closeable;
import java.io.PrintStream;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;

/**
 * Writes the text representation of the logic terms to a given output stream
 */
public class PrologPrintStream extends PrologStringWriter implements Closeable {

	protected final PrintStream out;

	public PrologPrintStream(Dialect dialect, OperatorsContext operatorsContext, PrintStream out) {
		super(dialect, operatorsContext);
		this.out = out;
	}

	@Override
	protected void println(String string) {
		out.println(string);
	}

	@Override
	public void close() {
		out.close();
	}
}
