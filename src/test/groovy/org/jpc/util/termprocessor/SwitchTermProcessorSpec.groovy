package org.jpc.util.termprocessor

import org.jpc.JpcException
import org.jpc.term.Term
import spock.lang.Specification

import static org.jpc.term.Atom.atom

class SwitchTermProcessorSpec extends Specification {

    def 'Throws exception when no defined processors'() {
        given:
        SwitchTermProcessor matchingTermProcessor = SwitchTermProcessor.builder().build()

        when:
        matchingTermProcessor.accept(atom("x"))

        then:
        thrown JpcException
    }

    def 'Throws exception when no matching processors'() {
        given:
        SwitchTermProcessor matchingTermProcessor = SwitchTermProcessor.builder()
                .addProcessor(atom("y"), Mock(TermProcessor))
                .build()

        when:
        matchingTermProcessor.accept(atom("x"))

        then:
        thrown JpcException
    }


    def 'Matching processor invoked'() {
        given:
        TermProcessor termProcessor = Mock()
        Term matchingTerm = atom("x")
        Term unmatchingTerm = atom("y")
        SwitchTermProcessor matchingTermProcessor = SwitchTermProcessor.builder()
                .addProcessor(matchingTerm, termProcessor)
                .addProcessor(unmatchingTerm, termProcessor)
                .build()

        when:
        matchingTermProcessor.accept(matchingTerm)

        then:
        1 * termProcessor.accept(matchingTerm)
    }

    def 'Only first matching processors invoked'() {
        given:
        TermProcessor firstTermProcessor = Mock()
        TermProcessor secondTermProcessor = Mock()
        TermProcessor thirdTermProcessor = Mock()
        Term matchingTerm = atom("x")
        Term unmatchingTerm = atom("y")
        SwitchTermProcessor matchingTermProcessor = SwitchTermProcessor.builder()
                .addProcessor(matchingTerm, firstTermProcessor)
                .addProcessor(matchingTerm, secondTermProcessor)
                .addProcessor(unmatchingTerm, thirdTermProcessor)
                .build()

        when:
        matchingTermProcessor.accept(matchingTerm)

        then:
        1 * firstTermProcessor.accept(matchingTerm)
        0 * secondTermProcessor.accept(matchingTerm)
        0 * thirdTermProcessor.accept(matchingTerm)
    }

}
