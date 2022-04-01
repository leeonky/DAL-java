package com.github.leeonky.interpreter;

import com.github.leeonky.dal.compiler.Notations;

public class BaseTest {
    public static SourceCode createSourceCode(String code) {
        return SourceCode.createSourceCode(code, Notations.LINE_COMMENTS);
    }

    protected TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(createSourceCode(s));
    }

    protected TestProcedure givenProcedureWithBlankCode(String s) {
        SourceCode sourceCode = createSourceCode("blabla" + s);
        sourceCode.popWord(Notation.notation("blabla"));
        return new TestProcedure(sourceCode);
    }
}
