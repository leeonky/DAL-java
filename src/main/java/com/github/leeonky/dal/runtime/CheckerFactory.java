package com.github.leeonky.dal.runtime;

import java.util.Optional;

public interface CheckerFactory {
    Optional<ConditionalChecker> create(Data expected, Data actual);

    default Data transformActual(Data actual) {
        return actual;
    }

    default Data transformExpected(Data expected) {
        return expected;
    }
}
