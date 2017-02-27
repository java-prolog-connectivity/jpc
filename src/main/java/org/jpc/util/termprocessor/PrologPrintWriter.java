package org.jpc.util.termprocessor;


import java.io.Closeable;
import java.io.PrintWriter;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;

public class PrologPrintWriter extends PrologStringWriter implements Closeable {

    protected final PrintWriter printWriter;

    public PrologPrintWriter(Dialect dialect, OperatorsContext operatorsContext, PrintWriter printWriter) {
        super(dialect, operatorsContext);
        this.printWriter = printWriter;
    }

    @Override
    protected void println(String string) {
        printWriter.println(string);
    }

    @Override
    public void close() {
        printWriter.close();
    }

}
