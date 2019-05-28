package com.github.leeonky.dal;

import com.github.leeonky.dal.token.SourceCode;

public class DataAssertor {
    private DALCompiler dalCompiler = new DALCompiler();
    private CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder();

    public CompilingContextBuilder getCompilingContextBuilder() {
        return compilingContextBuilder;
    }

    public AssertResult assertData(Object actual, String expression) {
        return (boolean) dalCompiler.compile(new SourceCode(expression)).evaluate(compilingContextBuilder.build(actual)) ?
                AssertResult.passedResult() :
                AssertResult.failedResult(actual, expression);
    }
}
