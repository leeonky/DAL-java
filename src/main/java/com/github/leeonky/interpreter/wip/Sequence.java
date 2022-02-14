package com.github.leeonky.interpreter.wip;

import com.github.leeonky.interpreter.Notation;
import com.github.leeonky.interpreter.Procedure;

import static java.lang.String.format;

public abstract class Sequence<P extends Procedure<?, ?, ?, ?, ?>> {

    public abstract void close(P procedure);

    public abstract boolean beforeClose(P procedure);

    public abstract boolean isSplitter(P procedure);

    public abstract static class DefaultSequence<P extends Procedure<?, ?, ?, ?, ?>> extends Sequence<P> {
        public static <P extends Procedure<?, ?, ?, ?, ?>> Sequence<P> endWith(Notation notation) {
            return new DefaultSequence<P>() {
                @Override
                public void close(P procedure) {
                    if (!procedure.getSourceCode().popWord(notation.getLabel()).isPresent())
                        throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
                }
            };
        }

        @Override
        public boolean beforeClose(P procedure) {
            return false;
        }

        @Override
        public boolean isSplitter(P procedure) {
            return false;
        }
    }
}
