package com.github.leeonky.interpreter;

public class BaseTest {
    protected TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }
}
