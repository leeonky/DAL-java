package com.github.leeonky.interpreter;

public class TestProcedure extends Procedure<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> {
    public TestProcedure(SourceCode sourceCode) {
        super(sourceCode, null, null);
    }

    public TestProcedure(SourceCode sourceCode,
                         ExpressionFactory<TestContext, TestNode, TestExpression, TestOperator> expressionFactory) {
        super(sourceCode, null, expressionFactory);
    }
}
