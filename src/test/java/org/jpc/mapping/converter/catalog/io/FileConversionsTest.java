package org.jpc.mapping.converter.catalog.io;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.mapping.converter.catalog.net.URIConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.junit.Test;

public class FileConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testFileConversion() {
		String fileName = "someFile";
		String uriFileName = "file:/"+fileName;
		URI uri;
		try {
			uri = new URI(uriFileName);
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
		File file = new File(uri);
		Compound fileCompound = jpc.toTerm(file);
		assertEquals(new Compound(FileConverter.FILE_FUNCTOR_NAME, asList(new Compound(URIConverter.URI_FUNCTOR_NAME, asList(new Atom(uriFileName))))), fileCompound);
		assertEquals(file, jpc.fromTerm(fileCompound));
	}
	
}
