package org.jpc.util.termprocessor;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;

public class PrologFileWriter extends PrologPrintWriter {

	protected File file;

	private static PrintWriter asPrintWriter(File file) {
		try {
			return new PrintWriter(new BufferedWriter(new FileWriter(file)));
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	public PrologFileWriter(Dialect dialect, OperatorsContext operatorsContext, File file) {
		super(dialect, operatorsContext, asPrintWriter(file));
		this.file = file;
	}

	@Override
	protected void println(String string) {
		super.println(escapeNewLines(string));
	}

	private static String escapeNewLines(String unescaped) {
		return unescaped.replaceAll("\n", "\\\\n");
	}

}
