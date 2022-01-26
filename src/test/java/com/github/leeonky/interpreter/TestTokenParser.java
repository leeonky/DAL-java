package com.github.leeonky.interpreter;

public class TestTokenParser extends TokenParser<TestContext, TestNode, TestExpression, TestOperator, TestTokenParser> {
    public TestTokenParser(SourceCode sourceCode) {
        super(sourceCode, null, null);
    }
}
