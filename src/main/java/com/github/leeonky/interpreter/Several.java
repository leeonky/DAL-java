package com.github.leeonky.interpreter;

import java.util.List;

public interface Several<P extends Procedure<?, ?, ?, ?, ?>> {

    static Several<Procedure<?, ?, ?, ?, ?>> severalTimes() {
        return new Several<Procedure<?, ?, ?, ?, ?>>() {
        };
    }

    static Several<Procedure<?, ?, ?, ?, ?>> atLeastOnce(String message) {
        return new Several<Procedure<?, ?, ?, ?, ?>>() {
            @Override
            public <T> List<T> validate(Procedure<?, ?, ?, ?, ?> procedure, List<T> list) {
                if (list.isEmpty())
                    throw procedure.getSourceCode().syntaxError(message, 0);
                return list;
            }
        };
    }

    default boolean isBreak(P procedure) {
        return false;
    }

    default <T> List<T> validate(P procedure, List<T> list) {
        return list;
    }

    default Several<P> splitBy(String s) {
        return new Several<P>() {
            @Override
            public boolean isBreak(P procedure) {
                return !procedure.getSourceCode().popWord(s).isPresent();
            }

            @Override
            public <T> List<T> validate(P procedure, List<T> list) {
                return Several.this.validate(procedure, list);
            }
        };
    }
}
