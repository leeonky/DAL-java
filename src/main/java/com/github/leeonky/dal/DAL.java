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

import java.util.*;
import java.util.stream.Collectors;

import static com.github.leeonky.util.Classes.subTypesOf;
import static com.github.leeonky.util.function.Extension.not;
import static java.util.Arrays.asList;
import static java.util.stream.Stream.concat;

public class DAL {
    private final Compiler compiler = new Compiler();
    private final RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();
    private static final ThreadLocal<DAL> instance = new ThreadLocal<>();

    public static synchronized DAL getInstance() {
        if (instance.get() == null)
            instance.set(create());
        return instance.get();
    }

    public static DAL create(Class<?>... exceptExtensions) {
        Iterator<DALFactory> iterator = ServiceLoader.load(DALFactory.class).iterator();
        if (iterator.hasNext())
            return iterator.next().newInstance();
        return new DAL().extend(exceptExtensions);
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
        List<DALNode> nodes = compiler.compile(new SourceCode(format(expression), Notations.LINE_COMMENTS),
                DALRuntimeContext);
        if (nodes.size() > 1)
            throw new SyntaxException("more than one expression", getOperandPosition(nodes.get(1)));
        return (T) nodes.get(0).evaluate(DALRuntimeContext);
    }

    private int getOperandPosition(DALNode node) {
        return node.getPositionBegin() == 0 ? node.getOperandPosition() : node.getPositionBegin();
    }

    private String format(String expression) {
        return String.join("\n", TextUtil.lines(expression));
    }

    public DAL extend(Class<?>... excepts) {
        Set<Class<?>> exceptExtensions = new HashSet<>(asList(excepts));
        concat(subTypesOf(Extension.class, "com.github.leeonky.dal.extensions").stream(),
                subTypesOf(Extension.class, "com.github.leeonky.extensions.dal").stream())
                .filter(not(exceptExtensions::contains))
                .map(Classes::newInstance)
                .sorted(Comparator.comparing(Extension::order))
                .forEach(e -> e.extend(this));
        return this;
    }
}
