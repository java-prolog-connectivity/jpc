package org.jpc.mapping.converter

import org.jpc.Jpc
import org.jpc.JpcBuilder
import spock.lang.Specification

import static org.jpc.term.Atom.atom

class TermDomainConversionSpec extends Specification {

    def 'A term conversion is influenced by the target domain'() {
        given:
        Jpc jpc = JpcBuilder.create()
                .register(new ConvertersFixture.HelloConverter())
                .register(new ConvertersFixture.ByeConverter())
                .build()

        expect:
        atom("x").equals(jpc.toTerm("x"))//TODO
    }

}
