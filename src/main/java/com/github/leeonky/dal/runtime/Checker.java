package com.github.leeonky.dal.runtime;

public interface Checker {
    boolean verify(CheckingContext checkingContext);

    default Checker and(Checker another) {
        return (expectActual) -> verify(expectActual) && another.verify(expectActual);
    }
}
