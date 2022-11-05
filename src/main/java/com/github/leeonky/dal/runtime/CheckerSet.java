package com.github.leeonky.dal.runtime;

import java.util.LinkedList;
import java.util.Optional;
import java.util.function.BiFunction;

public class CheckerSet {
    private final LinkedList<CheckerFactory> checkerFactories = new LinkedList<>();
    private final ClassKeyMap<ClassKeyMap<CheckerFactory>> typeTypeFactories = new ClassKeyMap<>();
    private final ClassKeyMap<CheckerFactory> typeFactories = new ClassKeyMap<>();
    private final BiFunction<Data, Data, Checker> defaultChecker;

    public CheckerSet(BiFunction<Data, Data, Checker> defaultChecker) {
        this.defaultChecker = defaultChecker;
    }

    public CheckerSet register(CheckerFactory factory) {
        checkerFactories.addFirst(factory);
        return this;
    }

    public CheckerSet register(Class<?> expected, Class<?> actual, CheckerFactory factory) {
        typeTypeFactories.computeIfAbsent(expected, _ignore -> new ClassKeyMap<>()).put(actual, factory);
        return this;
    }

    public CheckerSet register(Class<?> expected, CheckerFactory factory) {
        typeFactories.put(expected, factory);
        return this;
    }

    public Checker fetch(Data expected, Data actual) {
        return fetchByExpectedDataAndActualData(expected, actual).orElseGet(() ->
                fetchByExpectedTypeAndActualType(expected, actual).orElseGet(() ->
                        fetchByExpectedType(expected, actual).orElseGet(() ->
                                defaultChecker.apply(expected, actual))));
    }

    private Optional<Checker> fetchByExpectedType(Data expected, Data actual) {
        return typeFactories.tryGetData(expected.getInstance())
                .flatMap(factory -> factory.create(expected, actual));
    }

    private Optional<Checker> fetchByExpectedTypeAndActualType(Data expected, Data actual) {
        return typeTypeFactories.tryGetData(expected.getInstance())
                .flatMap(classKeyMap -> classKeyMap.tryGetData(actual.getInstance()))
                .flatMap(factory -> factory.create(expected, actual));
    }

    private Optional<Checker> fetchByExpectedDataAndActualData(Data expected, Data actual) {
        return checkerFactories.stream().map(factory -> factory.create(expected, actual))
                .filter(Optional::isPresent).map(Optional::get).findFirst();
    }

    public static Checker defaultMatching(Data expected, Data actual) {
        return expected.isNull() ? Checker.MATCH_NULL_CHECKER : Checker.MATCH_CHECKER;
    }

    public static Checker defaultEqualing(Data expected, Data actual) {
        return Checker.DEFAULT_EQUALS_CHECKER;
    }
}