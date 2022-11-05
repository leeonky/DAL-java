package com.github.leeonky.dal.runtime;

import java.util.Optional;

public interface CheckerFactory {
    Optional<Checker> create(Data expected, Data actual);
}
