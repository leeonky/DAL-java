package com.github.leeonky.interpreter;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
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
    public Optional<N> fetchNodeWithOneChildNodeBetween(
            char opening, NodeParser.Mandatory<C, N, E, O, P> childNodeMandatory, char closing,
            Function<N, N> nodeFactory, String message) {
        return fetchNodeWithElementsBetween(opening, () -> childNodeMandatory.parse(getInstance()), closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        });
    }

    @Deprecated
    public Optional<Clause<C, N>> fetchClauseBetween(
            char opening, NodeParser.Mandatory<C, N, E, O, P> childNodeMandatory, char closing,
            BiFunction<N, N, N> biNodeFactory, String message) {
        return fetchNodeWithOneChildNodeBetween(opening, childNodeMandatory, closing, n -> n, message).map(
                n -> previous -> biNodeFactory.apply(previous, n).setPositionBegin(n.getPositionBegin()));
    }

    @Deprecated
    public <T> Optional<N> fetchNodeWithElementsBetween(Character opening, Supplier<T> element, char closing,
                                                        Function<List<T>, N> nodeFactory) {
        return sourceCode.fetchElementNode(BY_NODE, opening, element, closing, nodeFactory);
    }

    public Optional<N> fetchString(Character opening, char closing, Function<String, N> nodeFactory,
                                   Map<String, Character> escapeChars) {
        return sourceCode.fetchElementNode(BY_CHAR, opening, () -> sourceCode.popChar(escapeChars), closing,
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

    @Deprecated
    public Optional<Clause<C, N>> fetchClauseAfter(
            String token, ClauseParser.Mandatory<C, N, E, O, P> clauseFactory) {
        return sourceCode.popWord(token).map(t -> clauseFactory.parse(getInstance())
                .map(node -> node.setPositionBegin(t.getPosition())));
    }

    @Deprecated
    public Optional<N> fetchExpression(N left, OperatorParser<C, N, E, O, P> operatorParser,
                                       NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return operatorParser.parse(getInstance()).map(opt -> (OperatorParser.Mandatory<C, N, E, O, P>) _ignore -> opt)
                .map(mandatoryParser -> fetchExpression(left, mandatoryParser, rightCompiler));
    }

    @Deprecated
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

    @Deprecated
    public Clause<C, N> fetchClause(OperatorParser.Mandatory<C, N, E, O, P> mandatoryParser,
                                    NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return fetchClause(mandatoryParser.parse(getInstance()), rightCompiler);
    }

    public Clause<C, N> fetchClause(O operator, NodeParser.Mandatory<C, N, E, O, P> rightMandatory) {
        operators.push(operator);
        try {
            N right = rightMandatory.parse(getInstance());
            return input -> expressionConstructor.newInstance(input, operator, right).adjustOperatorOrder(expressionConstructor);
        } finally {
            operators.pop();
        }
    }

    @Deprecated
    public Optional<Clause<C, N>> fetchClause(OperatorParser<C, N, E, O, P> operatorParser,
                                              NodeParser.Mandatory<C, N, E, O, P> rightCompiler) {
        return operatorParser.parse(getInstance()).map(operator -> fetchClause(operator, rightCompiler));
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