package com.github.leeonky.dal;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.compiler.Compiler;
import com.github.leeonky.dal.compiler.Notations;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.dal.util.TextUtil;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.SyntaxException;
import com.github.leeonky.util.Classes;

import java.util.List;
import java.util.stream.Collectors;

public class DAL {
    private final Compiler compiler = new Compiler();
    private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();
    private static final ThreadLocal<DAL> instance = new ThreadLocal<>();

    public static synchronized DAL getInstance() {
        if (instance.get() == null)
            instance.set(DALFactory.create());
        return instance.get();
    }

    public RuntimeContextBuilder getRuntimeContextBuilder() {
        return runtimeContextBuilder;
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> evaluateAll(Object input, String expressions) {
        DALRuntimeContext runtimeContext = runtimeContextBuilder.build(input);
        return compiler.compile(new SourceCode(format(expressions), Notations.LINE_COMMENTS), runtimeContext).stream()
                .map(node -> (T) node.evaluate(runtimeContext))
                .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    public <T> T evaluate(Object input, String expression) {
        DALRuntimeContext DALRuntimeContext = runtimeContextBuilder.build(input);
        List<DALNode> nodes = compiler.compile(new SourceCode(format(expression), Notations.LINE_COMMENTS), DALRuntimeContext);
        if (nodes.size() > 1)
            throw new SyntaxException("more than one expression", nodes.get(1).getPositionBegin());
        return (T) nodes.get(0).evaluate(DALRuntimeContext);
    }

    private String format(String expression) {
        return String.join("\n", TextUtil.lines(expression));
    }

    public DAL extend() {
        Classes.subTypesOf(Extension.class, "com.github.leeonky.dal.extensions")
                .forEach(c -> ((Extension) Classes.newInstance(c)).extend(this));
        return this;
    }
}
