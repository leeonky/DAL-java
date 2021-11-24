package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.compiler.SourceCode;
import com.github.leeonky.dal.compiler.SyntaxException;
import com.github.leeonky.dal.runtime.AssertResult;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.util.BeanClass.getClassName;

public class DAL {
    private final Compiler compiler = new Compiler();
    private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();
    private static DAL instance;

    public static DAL getInstance() {
        if (instance == null)
            instance = DALFactory.create();
        return instance;
    }

    public RuntimeContextBuilder getRuntimeContextBuilder() {
        return runtimeContextBuilder;
    }

    /**
     * Use evaluateAll instead
     */
    @Deprecated
    public AssertResult assertTrue(Object actual, String expression) {
        Object result = evaluate(actual, expression);
        if (result instanceof Boolean)
            return (boolean) result ? AssertResult.passedResult()
                    : AssertResult.failedResult(actual, expression);
        throw new IllegalStateException("Verification result should be boolean but '" + getClassName(result) + "'");
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> evaluateAll(Object input, String expressions) {
        return compiler.compile(new SourceCode(expressions)).stream()
                .map(node -> (T) node.evaluate(runtimeContextBuilder.build(input)))
                .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    public <T> T evaluate(Object input, String expression) {
        List<Node> nodes = compiler.compile(new SourceCode(expression));
        if (nodes.size() > 1)
            throw new SyntaxException("more than one expression", nodes.get(1).getPositionBegin());
        return (T) nodes.get(0).evaluate(runtimeContextBuilder.build(input));
    }
}
