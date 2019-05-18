package com.github.leeonky.dal;

public class DataAssertor {
    private DALCompiler dalCompiler = new DALCompiler();

    public AssertResult assertData(Object actual, String expression) {
        return dalCompiler.compile(expression).evaluate(new CompilingContext(actual));
    }
}
