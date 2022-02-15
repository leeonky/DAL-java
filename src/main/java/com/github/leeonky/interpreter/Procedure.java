package com.github.leeonky.interpreter;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.dal.compiler.Notations.COLUMN_SPLITTER;
import static com.github.leeonky.interpreter.IfThenFactory.when;

public class Procedure<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final ExpressionConstructor<C, N, E, O> expressionConstructor;

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

    public N createExpression(N node1, O operator, N node2) {
        return expressionConstructor.newInstance(node1, operator, node2).adjustOperatorOrder(expressionConstructor);
    }

    @Deprecated
    public <LE> Optional<List<LE>> fetchRow(Function<Integer, LE> factory) {
        return when(sourceCode.popWord(COLUMN_SPLITTER).isPresent()).optional(() -> new ArrayList<LE>() {{
            int col = 0;
            while (!sourceCode.isEndOfLine()) {
                add(factory.apply(col++));
                sourceCode.popWord(COLUMN_SPLITTER).orElseThrow(() -> sourceCode.syntaxError("Should end with `|`", 0));
            }
        }});
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }

    public Optional<O> currentOperator() {
        return operators.stream().findFirst();
    }
}
