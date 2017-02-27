package org.jpc.util.salt;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.UUID;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.util.JpcPreferences;
import org.jpc.util.termprocessor.PrologFileWriter;

public class CachedPrologEngineWriter extends PrologFileWriter implements Closeable {

	private static String getName() {
		return "prolog-cached-writer_" + UUID.randomUUID().toString();
	}

	private static String getTmpExtension() {
		return "tmp";
	}

	private PrologEngine prologEngine;

	public CachedPrologEngineWriter(PrologEngine prologEngine) throws IOException {
		this(prologEngine, new JpcPreferences());
	}
	
	public CachedPrologEngineWriter(PrologEngine prologEngine, JpcPreferences jpcPreferences) throws IOException {
		super(prologEngine.dialect(),
				OperatorsContext.empty(), // the final target is the engine so no need to pretty print operators.
				File.createTempFile(getName(), "." + getTmpExtension(), jpcPreferences.getJpcTmpDirectory()));
		this.prologEngine = prologEngine;
		//file.deleteOnExit();
	}
	
	public PrologEngine getPrologEngine() {
		return prologEngine;
	}

	@Override
	public void close() {
		super.close();
		getPrologEngine().ensureLoaded(file.getAbsolutePath());
	}

}
