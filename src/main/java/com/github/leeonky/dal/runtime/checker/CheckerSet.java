package com.github.leeonky.dal.runtime.checker;

import com.github.leeonky.dal.runtime.ClassKeyMap;
import com.github.leeonky.dal.runtime.Data;

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
        typeTypeFactories.get(expected, ClassKeyMap::new).put(actual, factory);
        return this;
    }

    public CheckerSet register(Class<?> expected, CheckerFactory factory) {
        typeFactories.put(expected, factory);
        return this;
    }

    public Checker fetch(Data expected, Data actual) {
        return fetchByExpectedDataAndActualData(expected, actual).orElseGet(() ->
                fetchByExpectedTypeAndActualType(expected, actual).orElseGet(() ->
                        fetchByExpectedType(expected, actual).orElseGet(() -> defaultChecker.apply(expected, actual))));
    }

    private Optional<Checker> fetchByExpectedType(Data expected, Data actual) {
        return typeFactories.tryGetData(expected.instance()).flatMap(factory -> factory.create(expected, actual));
    }

    private Optional<Checker> fetchByExpectedTypeAndActualType(Data expected, Data actual) {
        return typeTypeFactories.tryGetData(expected.instance())
                .flatMap(classKeyMap -> classKeyMap.tryGetData(actual.instance()))
                .flatMap(factory -> factory.create(expected, actual));
    }

    private Optional<Checker> fetchByExpectedDataAndActualData(Data expected, Data actual) {
        return checkerFactories.stream().map(factory -> factory.create(expected, actual))
                .filter(Optional::isPresent).map(Optional::get).findFirst();
    }

    public static Checker defaultMatching(Data expected, Data actual) {
        if (expected.isNull())
            return Checker.MATCH_NULL_CHECKER;
        return Checker.MATCHES_CHECKER;
    }

    public static Checker defaultEqualing(Data expected, Data actual) {
        return Checker.EQUALS_CHECKER;
    }
}