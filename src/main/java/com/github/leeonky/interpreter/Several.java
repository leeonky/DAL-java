package com.github.leeonky.interpreter;

import java.util.List;

public interface Several<P extends Procedure<?, ?, ?, ?, ?>> {

    abstract class DefaultSeveral<P extends Procedure<?, ?, ?, ?, ?>> implements Several<P> {
        @Override
        public boolean isClosing(P procedure) {
            return false;
        }

        @Override
        public boolean isSplitter(P procedure) {
            return true;
        }
    }

    class CompositeSeveral<P extends Procedure<?, ?, ?, ?, ?>> implements Several<P> {
        private final Several<P> several;

        public CompositeSeveral(Several<P> several) {
            this.several = several;
        }

        @Override
        public boolean isSplitter(P procedure) {
            return several.isSplitter(procedure);
        }

        @Override
        public boolean isClosing(P procedure) {
            return several.isClosing(procedure);
        }

        @Override
        public <T> List<T> validate(P procedure, List<T> list) {
            return several.validate(procedure, list);
        }

        @Override
        public Several<P> endWith(Notation notation) {
            return several.endWith(notation);
        }
    }

    static Several<Procedure<?, ?, ?, ?, ?>> severalTimes() {
        return new DefaultSeveral<Procedure<?, ?, ?, ?, ?>>() {
            @Override
            public <T> List<T> validate(Procedure<?, ?, ?, ?, ?> procedure, List<T> list) {
                return list;
            }
        };
    }

    boolean isSplitter(P procedure);

    boolean isClosing(P procedure);

    <T> List<T> validate(P procedure, List<T> list);

    default Several<P> splitBy(String s) {
        return new CompositeSeveral<P>(Several.this) {
            @Override
            public boolean isSplitter(P procedure) {
                return procedure.getSourceCode().popWord(s).isPresent();
            }
        };
    }

    default Several<P> splitByOptional(String s) {
        return new CompositeSeveral<P>(Several.this) {
            @Override
            public boolean isSplitter(P procedure) {
                procedure.getSourceCode().popWord(s);
                return true;
            }
        };
    }

    default Several<P> beforeEnd(String closing) {
        return new CompositeSeveral<P>(Several.this) {
            @Override
            public boolean isClosing(P procedure) {
                return procedure.getSourceCode().startsWith(closing);
            }
        };
    }

    default Several<P> endWith(Notation notation) {
        return new CompositeSeveral<P>(Several.this) {
            @Override
            public boolean isClosing(P procedure) {
                if (procedure.getSourceCode().startsWithStr(notation.getLabel())) {
                    procedure.getSourceCode().popWord(notation.getLabel());
                    return true;
                }
                return false;
            }
        };
    }
}
