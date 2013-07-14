package org.jpc.salt;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

public class PrologFileWriter extends PrologStreamWriter {

	protected File file;
	
	public PrologFileWriter(File file) throws FileNotFoundException {
		super(new PrintStream(file));
		this.file = file;
	}
}
