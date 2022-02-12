package com.github.leeonky.interpreter;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static java.util.stream.Collectors.joining;

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

    @Deprecated
    public Optional<N> fetchString(Character opening, char closing, Function<String, N> nodeFactory,
                                   Map<String, Character> escapeChars) {
        return sourceCode.fetchElementNode(BY_CHAR, opening, () -> sourceCode.popChar(escapeChars), closing,
                chars -> nodeFactory.apply(chars.stream().map(String::valueOf).collect(joining())));
    }

    @Deprecated
    public Optional<N> fetchNodeBetween(String opening, String closing, NodeParser<C, N, E, O, P> nodeParser) {
        return sourceCode.tryFetch(() -> {
            Optional<N> optionalNode = Optional.empty();
            if (sourceCode.popWord(opening).isPresent()) {
                optionalNode = nodeParser.parse(getInstance());
                if (optionalNode.isPresent())
                    sourceCode.popWord(closing).orElseThrow(() ->
                            sourceCode.syntaxError("should end with `" + closing + "`", 0));
            }
            return optionalNode;
        });
    }

    @Deprecated
    public Optional<N> fetchNodeAfter(String token, NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.parse(getInstance()));
    }

    @SuppressWarnings("unchecked")
    private P getInstance() {
        return (P) this;
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
        return when(sourceCode.popWord("|").isPresent()).optional(() -> new ArrayList<LE>() {{
            int col = 0;
            while (!sourceCode.isEndOfLine()) {
                add(factory.apply(col++));
                sourceCode.popWord("|").orElseThrow(() -> sourceCode.syntaxError("Should end with `|`", 0));
            }
        }});
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }

    public Optional<O> currentOperator() {
        return operators.stream().findFirst();
    }

    public <T> T atPosition(Function<Integer, T> function) {
        return function.apply(sourceCode.nextPosition());
    }
}
