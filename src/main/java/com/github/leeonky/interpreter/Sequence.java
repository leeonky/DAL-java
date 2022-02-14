package com.github.leeonky.interpreter;

import java.util.List;

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

    public static <P extends Procedure<?, ?, ?, ?, ?>> Sequence<P> atLeast(int count, String message) {
        return new DefaultSequence<P>() {
            @Override
            public <T> List<T> validate(P procedure, List<T> list) {
                if (list.size() < count)
                    throw procedure.getSourceCode().syntaxError(message, -1);
                return list;
            }
        };
    }


    public Sequence<P> splitBy(Notation notation) {
        return new CompositeSequence<P>(this) {

            @Override
            public boolean isSplitter(P procedure) {
                return procedure.getSourceCode().popWord(notation.getLabel()).isPresent();
            }
        };
    }

    public Sequence<P> endWith(Notation notation) {
        return new CompositeSequence<P>(endWith(notation.getLabel())) {
            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(notation.getLabel());
            }
        };
    }

    public Sequence<P> endWith(String closing) {
        return new CompositeSequence<P>(this) {

            @Override
            public void close(P procedure) {
                if (!procedure.getSourceCode().popWord(closing).isPresent())
                    throw procedure.getSourceCode().syntaxError(format("should end with `%s`", closing), 0);
            }

            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWithStr(closing);
            }
        };
    }

    public Sequence<P> optionalSplitBy(Notation splitter) {
        return new CompositeSequence<P>(this) {

            @Override
            public boolean isSplitter(P procedure) {
                procedure.getSourceCode().popWord(splitter.getLabel());
                return true;
            }
        };
    }
}
