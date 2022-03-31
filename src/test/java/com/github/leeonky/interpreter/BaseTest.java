package com.github.leeonky.interpreter;

public class BaseTest {
    protected TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }

    protected TestProcedure givenProcedureWithBlankCode(String s) {
        SourceCode sourceCode = new SourceCode("blabla" + s);
        sourceCode.popWord(Notation.notation("blabla"));
        return new TestProcedure(sourceCode);
    }
}
