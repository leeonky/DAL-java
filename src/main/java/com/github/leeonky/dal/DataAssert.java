package com.github.leeonky.dal;

import com.github.leeonky.dal.token.SourceCode;

import static com.github.leeonky.util.BeanClass.getClassName;

public class DataAssert {
    private DALCompiler dalCompiler = new DALCompiler();
    private CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder();

    public CompilingContextBuilder getCompilingContextBuilder() {
        return compilingContextBuilder;
    }

    public AssertResult assertData(Object actual, String expression) {
        Object result = dalCompiler.compile(new SourceCode(expression)).evaluate(compilingContextBuilder.build(actual));
        if (result instanceof Boolean)
            return (boolean) result ? AssertResult.passedResult()
                    : AssertResult.failedResult(actual, expression);
        throw new IllegalStateException("Verification result should be boolean but '" + getClassName(result) + "'");
    }
}
