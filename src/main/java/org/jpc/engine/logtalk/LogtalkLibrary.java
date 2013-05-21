package org.jpc.engine.logtalk;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jpc.resource.LogtalkResource;

public class LogtalkLibrary {

	private static Map<String, LogtalkLibrary> defaultLogtalkLibraries;
	
	public static Map<String, LogtalkLibrary> getDefaultLogtalkLibraries() {
		return defaultLogtalkLibraries;
	}
	
	public static void setDefaultLogtalkLibraries(Map<String, LogtalkLibrary> defaultLogtalkLibraries) {
		LogtalkLibrary.defaultLogtalkLibraries = defaultLogtalkLibraries;
	}
	
	private LogtalkLibraryDescription libraryDescription;
	private Map<String,LogtalkLibraryItem> items;
	
	public LogtalkLibrary(LogtalkLibraryDescription libraryDescription) {
		this.libraryDescription = libraryDescription;
		items = new HashMap<>();
		initialize();
	}
	
	public LogtalkLibraryDescription getLibraryDescription() {
		return libraryDescription;
	}
	
	public String getAlias() {
		return libraryDescription.getAlias();
	}
	
	private void initialize() {
		List<String> libraryNames = new ArrayList<>();
		FileFilter fileFilter = new FileFilter() {
			@Override
			public boolean accept(File file) {
				return file.isFile() && LogtalkResource.hasLogtalkExtension(file.getName());
			}
		};
		File dir = new File(libraryDescription.getDirUri());
		for(File file : dir.listFiles(fileFilter)) {
			String name = file.getName();
			int indexExtension = name.lastIndexOf(".");
			name = name.substring(0, indexExtension);
			libraryNames.add(name);
		}
		Collections.sort(libraryNames);
		for(String name : libraryNames) {
			items.put(name, new LogtalkLibraryItem(this, name));  //TODO gather metadata from the library item file (e.g., from the Logtalk info directive) and pass it to the constructor.
		}
	}
	
	public Set<LogtalkLibraryItem> getItems() {
		return new HashSet<>(items.values());
	}

	public LogtalkLibraryItem getItem(String name) {
		return items.get(name);
	}

}
