package com.github.leeonky.interpreter;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
import static java.util.stream.Collectors.joining;

public class Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Parser<C, N, E, O, P>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final ExpressionConstructor<C, N, E, O> expressionConstructor;

    public Parser(SourceCode sourceCode, C runtimeContext, ExpressionConstructor<C, N, E, O> expressionConstructor) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionConstructor = expressionConstructor;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public Optional<N> fetchNodeWithOneChildNodeBetween(char opening, char closing, Function<N, N> nodeFactory,
                                                        NodeParser.Mandatory<C, N, E, O, P> childNodeFactory, String message) {
        return fetchNodeWithElementsBetween(opening, closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        }, () -> childNodeFactory.parse(getInstance()));
    }

    public Optional<ExpressionClause<C, N>> fetchExpressionClauseBetween(
            char opening, char closing, BiFunction<N, N, N> biNodeFactory,
            NodeParser.Mandatory<C, N, E, O, P> childNodeFactory, String message) {
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

    public List<N> fetchNodesSplitBy(String delimiter, NodeParser.Mandatory<C, N, E, O, P> factory) {
        return new ArrayList<N>() {{
            add(factory.parse(getInstance()));
            while (sourceCode.popWord(delimiter).isPresent())
                add(factory.parse(getInstance()));
        }};
    }

    public Optional<N> fetchNodeAfter2(String token, NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.parse(getInstance()));
    }

    @SuppressWarnings("unchecked")
    private P getInstance() {
        return (P) this;
    }

    public Optional<ExpressionClause<C, N>> fetchClauseAfter(
            String token, ExpressionClauseParser.ExpressionClauseFactory<C, N, E, O, P> clauseFactory) {
        return sourceCode.popWord(token).map(t -> clauseFactory.parse(getInstance())
                .map(node -> node.setPositionBegin(t.getPosition())));
    }

    public Optional<N> fetchExpression(N left, OperatorParser<C, N, E, O, P> operatorParser,
                                       NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return operatorParser.parse(getInstance()).map(opt -> (OperatorParser.Mandatory<C, N, E, O, P>) _ignore -> opt)
                .map(mandatoryParser -> fetchExpression(left, mandatoryParser, rightCompiler));
    }

    public N fetchExpression(N left, OperatorParser.Mandatory<C, N, E, O, P> mandatoryParser,
                             NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        O operator = mandatoryParser.parse(getInstance());
        operators.push(operator);
        try {
            return expressionConstructor.newInstance(left, operator, rightCompiler.parse(getInstance()))
                    .adjustOperatorOrder(expressionConstructor);
        } finally {
            operators.pop();
        }
    }

    public ExpressionClause<C, N> fetchExpressionClause(OperatorParser.Mandatory<C, N, E, O, P> mandatoryParser,
                                                        NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return fetchExpressionClause(mandatoryParser.parse(getInstance()), rightCompiler);
    }

    private ExpressionClause<C, N> fetchExpressionClause(O operator, NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        operators.push(operator);
        N right;
        try {
            right = rightCompiler.parse(getInstance());
        } finally {
            operators.pop();
        }
        return input -> expressionConstructor.newInstance(input, operator, right).adjustOperatorOrder(expressionConstructor);
    }

    public Optional<ExpressionClause<C, N>> fetchExpressionClause(OperatorParser<C, N, E, O, P> operatorParser,
                                                                  NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return operatorParser.parse(getInstance()).map(operator -> fetchExpressionClause(operator, rightCompiler));
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
