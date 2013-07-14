package org.jpc.salt;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;

import org.jpc.JpcPreferences;
import org.jpc.engine.prolog.PrologEngine;

public class CachedPrologEngineWriter extends PrologFileWriter implements Closeable {

	private PrologEngine prologEngine;
	
	public CachedPrologEngineWriter(PrologEngine prologEngine) throws IOException {
		this(prologEngine, new JpcPreferences());
	}
	
	public CachedPrologEngineWriter(PrologEngine prologEngine, JpcPreferences jpcPreferences) throws IOException {
		super(File.createTempFile("prolog-engine-writer", ".tmp", jpcPreferences.getJpcTmpDirectory()));
		this.prologEngine = prologEngine;
		//file.deleteOnExit();
	}
	
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}

	@Override
	public void close() throws IOException {
		out.close();
		getPrologEngine().ensureLoaded(file.getAbsolutePath());
	}

}
