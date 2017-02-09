package org.jpc.util.termprocessor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;

public class PrologFileWriter extends PrologPrintStreamWriter {

	protected File file;

	private static PrintStream asPrintStream(File file) {
		try {
			return new PrintStream(file);
		} catch(FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	public PrologFileWriter(Dialect dialect, OperatorsContext operatorsContext, File file) {
		super(dialect, operatorsContext, asPrintStream(file));
		this.file = file;
	}

}
