package com.github.leeonky.interpreter;

import java.util.List;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.lang.String.format;

public abstract class Sequence<P extends Procedure<?, ?, ?, ?, ?>> {

    public abstract void close(P procedure);

    public abstract boolean isClose(P procedure);

    public abstract boolean isSplitter(P procedure);

    public abstract <T> List<T> validate(P procedure, List<T> list);

    public static class DefaultSequence<P extends Procedure<?, ?, ?, ?, ?>> extends Sequence<P> {

        @Override
        public void close(P procedure) {
        }

        @Override
        public boolean isClose(P procedure) {
            return false;
        }

        @Override
        public boolean isSplitter(P procedure) {
            return true;
        }

        @Override
        public <T> List<T> validate(P procedure, List<T> list) {
            return list;
        }
    }

    public static class CompositeSequence<P extends Procedure<?, ?, ?, ?, ?>> extends Sequence<P> {
        private final Sequence<P> sequence;

        public CompositeSequence(Sequence<P> sequence) {
            this.sequence = sequence;
        }

        @Override
        public void close(P procedure) {
            sequence.close(procedure);
        }

        @Override
        public boolean isClose(P procedure) {
            return sequence.isClose(procedure);
        }

        @Override
        public boolean isSplitter(P procedure) {
            return sequence.isSplitter(procedure);
        }

        @Override
        public <T> List<T> validate(P procedure, List<T> list) {
            return sequence.validate(procedure, list);
        }
    }

    public static <P extends Procedure<?, ?, ?, ?, ?>> Sequence<P> severalTimes() {
        return new DefaultSequence<>();
    }

    public Sequence<P> endWith(Notation notation) {
        return new CompositeSequence<P>(this) {
            @Override
            public void close(P procedure) {
                if (!procedure.getSourceCode().popWord(notation).isPresent())
                    throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
            }

            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(notation);
            }
        };
    }

    public Sequence<P> endWith(String closing) {
        return new CompositeSequence<P>(endWith(notation(closing))) {
            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(closing);
            }
        };
    }

    public Sequence<P> endWithLine() {
        return new CompositeSequence<P>(this) {
            private boolean isClose = false;

            @Override
            public boolean isClose(P procedure) {
                //                TODO need test
                return isClose = procedure.getSourceCode().isEndOfLine();
            }

            @Override
            public void close(P procedure) {
                if (!isClose)
//                TODO need test
                    throw procedure.getSourceCode().syntaxError("unexpected token", 0);
            }
        };
    }

    public Sequence<P> mandatoryTailSplitBy(Notation notation) {
        return new CompositeSequence<P>(this) {

            @Override
            public boolean isSplitter(P procedure) {
                if (procedure.getSourceCode().popWord(notation).isPresent())
                    return true;
                throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
            }
        };
    }
}
