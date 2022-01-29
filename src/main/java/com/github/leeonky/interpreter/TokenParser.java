package com.github.leeonky.interpreter;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
import static java.util.stream.Collectors.joining;

public class TokenParser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final ExpressionConstructor<C, N, E, O> expressionConstructor;

    public TokenParser(SourceCode sourceCode, C runtimeContext, ExpressionConstructor<C, N, E, O> expressionConstructor) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionConstructor = expressionConstructor;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public Optional<N> fetchNodeWithOneChildNodeBetween(char opening, char closing, Function<N, N> nodeFactory,
                                                        NodeFactory<C, N, E, O, T> childNodeFactory, String message) {
        return fetchNodeWithElementsBetween(opening, closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        }, () -> childNodeFactory.fetch(getInstance()));
    }

    public Optional<ExpressionClause<C, N>> fetchExpressionClauseBetween(
            char opening, char closing, BiFunction<N, N, N> biNodeFactory,
            NodeFactory<C, N, E, O, T> childNodeFactory, String message) {
        return fetchNodeWithOneChildNodeBetween(opening, closing, n -> n, childNodeFactory, message).map(
                n -> previous -> biNodeFactory.apply(previous, n).setPositionBegin(n.getPositionBegin()));
    }

    public <T> Optional<N> fetchNodeWithElementsBetween(Character opening, char closing, Function<List<T>, N> nodeFactory,
                                                        Supplier<T> element) {
        return sourceCode.fetchElementNode(BY_NODE, opening, closing, element, nodeFactory);
    }

    public Optional<N> fetchString(Character opening, char closing, Function<String, N> nodeFactory,
                                   Map<String, Character> escapeChars) {
        return sourceCode.fetchElementNode(BY_CHAR, opening, closing, () -> sourceCode.popChar(escapeChars),
                chars -> nodeFactory.apply(chars.stream().map(String::valueOf).collect(joining())));
    }

    public <T> Optional<T> fetchBetween(String opening, String closing, Supplier<T> supplier) {
        return sourceCode.popWord(opening).map(token -> {
            T result = supplier.get();
            sourceCode.popWord(closing)
                    .orElseThrow(() -> sourceCode.syntaxError("should end with `" + closing + "`", 0));
            return result;
        });
    }

    public Optional<N> fetchNodeBetween(String opening, String closing, NodeMatcher<C, N, E, O, T> nodeMatcher) {
        return sourceCode.tryFetch(() -> {
            Optional<N> optionalNode = Optional.empty();
            if (sourceCode.popWord(opening).isPresent()) {
                optionalNode = nodeMatcher.fetch(getInstance());
                if (optionalNode.isPresent())
                    sourceCode.popWord(closing).orElseThrow(() ->
                            sourceCode.syntaxError("should end with `" + closing + "`", 0));
            }
            return optionalNode;
        });
    }

    public List<N> fetchNodesSplitBy(String delimiter, NodeFactory<C, N, E, O, T> factory) {
        return new ArrayList<N>() {{
            add(factory.fetch(getInstance()));
            while (sourceCode.popWord(delimiter).isPresent())
                add(factory.fetch(getInstance()));
        }};
    }

    public Optional<N> fetchNodeAfter2(String token, NodeFactory<C, N, E, O, T> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.fetch(getInstance()));
    }

    public Optional<N> fetchNodeAfter(String token, NodeFactory<C, N, E, O, T> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.fetch(getInstance()).setPositionBegin(t.getPosition()));
    }

    @SuppressWarnings("unchecked")
    private T getInstance() {
        return (T) this;
    }

    public Optional<ExpressionClause<C, N>> fetchClauseAfter(
            String token, ExpressionClauseFactory<C, N, E, O, T> clauseFactory) {
        return sourceCode.popWord(token).map(t -> clauseFactory.fetch(getInstance())
                .map(node -> node.setPositionBegin(t.getPosition())));
    }

    public Optional<N> fetchExpression(N left, OperatorMatcher<C, N, E, O, T> operatorMatcher,
                                       NodeFactory<C, N, E, O, T> rightCompiler) {
        return operatorMatcher.fetch(getInstance()).map(opt -> (OperatorFactory<C, N, E, O, T>) _ignore -> opt)
                .map(operatorFactory -> fetchExpression(left, operatorFactory, rightCompiler));
    }

    public N fetchExpression(N left, OperatorFactory<C, N, E, O, T> operatorFactory,
                             NodeFactory<C, N, E, O, T> rightCompiler) {
        O operator = operatorFactory.fetch(getInstance());
        operators.push(operator);
        try {
            return expressionConstructor.newInstance(left, operator, rightCompiler.fetch(getInstance()))
                    .adjustOperatorOrder(expressionConstructor);
        } finally {
            operators.pop();
        }
    }

    public ExpressionClause<C, N> fetchExpressionClause(OperatorFactory<C, N, E, O, T> operatorFactory,
                                                        NodeFactory<C, N, E, O, T> rightCompiler) {
        return fetchExpressionClause(operatorFactory.fetch(getInstance()), rightCompiler);
    }

    private ExpressionClause<C, N> fetchExpressionClause(O operator, NodeFactory<C, N, E, O, T> rightCompiler) {
        operators.push(operator);
        N right;
        try {
            right = rightCompiler.fetch(getInstance());
        } finally {
            operators.pop();
        }
        return input -> expressionConstructor.newInstance(input, operator, right).adjustOperatorOrder(expressionConstructor);
    }

    public Optional<ExpressionClause<C, N>> fetchExpressionClause(OperatorMatcher<C, N, E, O, T> operatorMatcher,
                                                                  NodeFactory<C, N, E, O, T> rightCompiler) {
        return operatorMatcher.fetch(getInstance()).map(operator -> fetchExpressionClause(operator, rightCompiler));
    }

    public Optional<N> wordToken(String word, Function<Token, N> factory) {
        return sourceCode.popWord(word).map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }

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
}
