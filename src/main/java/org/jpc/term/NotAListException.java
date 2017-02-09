package org.jpc.term;


import org.jpc.JpcException;

public class NotAListException extends JpcException {

    private final Term term;

    public NotAListException(Term term) {
        this.term = term;
    }

    @Override
    public String getMessage() {
        return "The term: " + term.toString() + " is not a list.";
    }
}
