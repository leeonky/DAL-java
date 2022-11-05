package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.Data;

import java.util.Optional;

public interface CheckerFactory {
    Optional<Checker> create(Data expected, Data actual);
}
