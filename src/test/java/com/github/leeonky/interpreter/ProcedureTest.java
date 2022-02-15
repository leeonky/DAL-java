package com.github.leeonky.interpreter;

class ProcedureTest {
    private TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }
}