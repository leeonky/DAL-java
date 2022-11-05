package com.github.leeonky.dal.runtime;

import java.util.function.BiFunction;

public class CheckerSet {
    private final ClassKeyMap<ClassKeyMap<CheckerFactory>> checkerFactories = new ClassKeyMap<>();
    private final ClassKeyMap<Checker> checkers = new ClassKeyMap<>();
    private final BiFunction<Data, Data, Checker> defaultChecker;

    public CheckerSet(BiFunction<Data, Data, Checker> defaultChecker) {
        this.defaultChecker = defaultChecker;
    }

    public CheckerSet register(Class<?> expected, Checker checker) {
        checkers.put(expected, checker);
        return this;
    }

    public CheckerSet register(Class<?> expected, Class<?> actual, CheckerFactory factory) {
        checkerFactories.computeIfAbsent(expected, _ignore -> new ClassKeyMap<>()).put(actual, factory);
        return this;
    }

    public Checker fetch(Data expected, Data actual) {
        //                     TODO default check for any expected and actual type
        return checkerFactories.tryGetData(expected.getInstance())
                .flatMap(classKeyMap -> classKeyMap.tryGetData(actual.getInstance()))
                .flatMap(factory -> factory.create(expected, actual))
                .orElseGet(() -> checkers.tryGetData(expected.getInstance())
                        .orElseGet(() -> defaultChecker.apply(expected, actual)));
    }

    public static Checker defaultMatching(Data expected, Data actual) {
        return expected.isNull() ? Checker.MATCH_NULL_CHECKER : Checker.MATCH_CHECKER;
    }

    public static Checker defaultEqualing(Data expected, Data actual) {
        return Checker.DEFAULT_EQUALS_CHECKER;
    }
}