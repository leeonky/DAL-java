package com.github.leeonky.dal.runtime;

public interface Checker {
    boolean verify(ExpectActual expectActual, int position);

    default Checker and(Checker another) {
        return (expectActual, position) -> verify(expectActual, position) && another.verify(expectActual, position);
    }
}
