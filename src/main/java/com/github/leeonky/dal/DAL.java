package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.util.BeanClass;

import java.util.List;
import java.util.stream.Collectors;

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

    @SuppressWarnings("unchecked")
    public <T> List<T> evaluateAll(Object input, String expressions) {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = runtimeContextBuilder.build(input);
        return compiler.compile(SourceCode.createSourceCode(expressions, Notations.LINE_COMMENTS), DALRuntimeContext).stream()
                .map(node -> (T) node.evaluate(DALRuntimeContext))
                .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    public <T> T evaluate(Object input, String expression) {
        RuntimeContextBuilder.DALRuntimeContext DALRuntimeContext = runtimeContextBuilder.build(input);
        List<DALNode> nodes = compiler.compile(SourceCode.createSourceCode(expression, Notations.LINE_COMMENTS), DALRuntimeContext);
        if (nodes.size() > 1)
            throw new SyntaxException("more than one expression", nodes.get(1).getPositionBegin());
        return (T) nodes.get(0).evaluate(DALRuntimeContext);
    }

    public DAL extend() {
        BeanClass.subTypesOf(Extension.class, "com.github.leeonky.dal.extensions")
                .forEach(c -> ((Extension) BeanClass.newInstance(c)).extend(this));
        return this;
    }
}
