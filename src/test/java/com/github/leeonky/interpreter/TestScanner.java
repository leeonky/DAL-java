package com.github.leeonky.interpreter;

public class TestScanner extends Scanner<TestContext, TestNode, TestExpression, TestOperator, TestScanner> {
    public TestScanner(SourceCode sourceCode) {
        super(sourceCode, null, null);
    }
}
