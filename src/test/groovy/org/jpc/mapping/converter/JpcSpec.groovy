package org.jpc.mapping.converter

import org.jpc.Jpc
import org.jpc.JpcBuilder
import org.jpc.term.Compound
import spock.lang.Specification

import static java.util.Arrays.asList
import static org.jpc.term.Atom.atom
import static org.jpc.term.Compound.compound

class JpcSpec extends Specification {


    def 'The conversion depends on the context id'() {
        given:
        Object bye = "BYE"
        Jpc defaultContext = JpcBuilder.create()
                .register(new ConvertersFixture.HelloConverter())
                .register(bye, new ConvertersFixture.ByeConverter())
                .build()
        Jpc byeContext = defaultContext.withId(bye)

        expect:
        defaultContext.toTerm("x", Compound.class).equals(compound("hello", asList(atom("x"))))
        byeContext.toTerm("x", Compound.class).equals(compound("bye", asList(atom("x"))))
    }

}
