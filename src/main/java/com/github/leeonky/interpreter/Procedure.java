package com.github.leeonky.interpreter;

import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;

public class Procedure<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final ExpressionFactory<C, N, E, O> expressionFactory;
    private final LinkedList<AtomicInteger> columns = new LinkedList<>();

    public Procedure(SourceCode sourceCode, C runtimeContext, ExpressionFactory<C, N, E, O> expressionFactory) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionFactory = expressionFactory;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public <T> T underOperator(O operator, Supplier<T> action) {
        operators.push(operator);
        try {
            return action.get();
        } finally {
            operators.pop();
        }
    }

    public <T> T positionOf(Function<Integer, T> action) {
        return action.apply(getSourceCode().nextPosition());
    }

    public <T> T withIndex(Supplier<T> action) {
        columns.push(new AtomicInteger());
        try {
            return action.get();
        } finally {
            columns.poll();
        }
    }

    public int getIndex() {
        return columns.getFirst().get();
    }

    public void incrementIndex() {
        columns.getFirst().incrementAndGet();
    }

    public N createExpression(N node1, O operator, N node2) {
        return expressionFactory.create(node1, operator, node2).applyPrecedence(expressionFactory);
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }

    public Optional<O> currentOperator() {
        return operators.stream().findFirst();
    }
}
