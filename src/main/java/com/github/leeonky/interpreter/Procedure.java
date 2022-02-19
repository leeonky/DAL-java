package com.github.leeonky.interpreter;

import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;

public class Procedure<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final ExpressionConstructor<C, N, E, O> expressionConstructor;
    private final LinkedList<AtomicInteger> columns = new LinkedList<>();

    public Procedure(SourceCode sourceCode, C runtimeContext, ExpressionConstructor<C, N, E, O> expressionConstructor) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionConstructor = expressionConstructor;
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

    public <T> T actionUnderIndex(Supplier<T> action) {
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
        return expressionConstructor.newInstance(node1, operator, node2).adjustOperatorOrder(expressionConstructor);
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }

    public Optional<O> currentOperator() {
        return operators.stream().findFirst();
    }
}
