package com.github.leeonky.interpreter;

public class TestProcedure extends Procedure<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> {
    public TestProcedure(SourceCode sourceCode) {
        super(sourceCode, null, null);
    }
}
