package org.jpc.engine.logtalk;

import static java.util.Arrays.asList;

import java.util.HashMap;
import java.util.Map;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;
import org.jpc.term.Compound;

/**
 * A pre-defined Logtalk library.
 * @author sergioc
 *
 */
public class LogtalkLibraryItem implements TermConvertable<Compound> {

	private LogtalkLibrary logtalkLibrary;
	private String name;
	private Map<String, String> entityInfo;

	public LogtalkLibraryItem(LogtalkLibrary logtalkLibrary, String name) {
		this(logtalkLibrary, name, new HashMap<String, String>());
	}
	
	public LogtalkLibraryItem(LogtalkLibrary logtalkLibrary, String name, Map<String, String> entityInfo) {
		this.logtalkLibrary = logtalkLibrary;
		this.name = name;
		this.entityInfo = entityInfo;
	}
	
	public String getName() {
		return name;
	}

	public String getProperty(String key) {
		return entityInfo.get(key);
	}
	
	@Override
	public Compound asTerm() {
		return new Compound(logtalkLibrary.getAlias(), asList(new Atom(name)));
	}

}
