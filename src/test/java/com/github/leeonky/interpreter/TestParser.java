package com.github.leeonky.interpreter;

public class TestParser extends Parser<TestContext, TestNode, TestExpression, TestOperator, TestParser> {
    public TestParser(SourceCode sourceCode) {
        super(sourceCode, null, null);
    }
}
