package com.github.leeonky.dal;

public class DataAssertor {
    private DALCompiler dalCompiler = new DALCompiler();

    public AssertResult assertData(Object actual, String expression) {
        return (boolean) dalCompiler.compile(new SourceCode(expression)).evaluate(new CompilingContext(actual)) ?
                AssertResult.passedResult() :
                AssertResult.failedResult(actual, expression);
    }

}
