package org.jpc.mapping.converter.catalog.io;

import static java.util.Arrays.asList;

import java.io.File;
import java.net.URI;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.util.PrologSpeakingClass;

public class FileConverter implements ToTermConverter<File, Compound>, FromTermConverter<Compound, File> {

	public static final String FILE_FUNCTOR_NAME = "file";
	
	@Override
	public File fromTerm(Compound compound, TypeDomain target, Jpc jpc) {
		PrologSpeakingClass<File> prologSpeakingClass = new PrologSpeakingClass<>(File.class, jpc);
		return prologSpeakingClass.newInstance(compound.getArgs());
	}

	@Override
	public Compound toTerm(File file, TypeDomain target, Jpc jpc) {
		URI uri = file.toURI();
		Term uriTerm = jpc.toTerm(uri);
		return new Compound(FILE_FUNCTOR_NAME, asList(uriTerm));
	}

}
