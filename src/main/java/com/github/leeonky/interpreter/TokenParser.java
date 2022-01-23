package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.SequenceNode;

import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
import static java.util.Collections.singleton;
import static java.util.stream.Collectors.joining;

public class TokenParser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>> {

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<O> operators = new LinkedList<>();
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));
    private final ExpressionConstructor<C, N, E, O> expressionConstructor;

    public TokenParser(SourceCode sourceCode, C runtimeContext, ExpressionConstructor<C, N, E, O> expressionConstructor) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionConstructor = expressionConstructor;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public Optional<N> fetchNode(char opening, char closing, Function<N, N> nodeFactory,
                                 NodeFactory<C, N, E, O> nodeMatcher, String message) {
        return fetchNodes(opening, closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        }, () -> nodeMatcher.fetch(this));
    }

    public <T> Optional<N> fetchNodes(Character opening, char closing, Function<List<T>, N> nodeFactory,
                                      Supplier<T> element) {
        return sourceCode.fetchElementNode(BY_NODE, opening, closing, element, nodeFactory);
    }

    public Optional<N> fetchString(Character opening, char closing, Function<String, N> nodeFactory,
                                   Map<String, Character> escapeChars) {
        return sourceCode.fetchElementNode(BY_CHAR, opening, closing, () -> sourceCode.escapedPop(escapeChars),
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

    public Optional<N> fetchBetween(String opening, String closing, NodeMatcher<C, N, E, O> nodeMatcher) {
        return sourceCode.tryFetch(() -> {
            Optional<N> optionalNode = Optional.empty();
            if (sourceCode.popWord(opening).isPresent()) {
                optionalNode = nodeMatcher.fetch(this);
                if (optionalNode.isPresent())
                    sourceCode.popWord(closing).orElseThrow(() ->
                            sourceCode.syntaxError("should end with `" + closing + "`", 0));
            }
            return optionalNode;
        });
    }

    public Optional<N> disableCommaAnd(Supplier<Optional<N>> nodeFactory) {
        return commaAnd(false, nodeFactory);
    }

    public Optional<N> enableCommaAnd(Supplier<Optional<N>> nodeFactory) {
        return commaAnd(true, nodeFactory);
    }

    private Optional<N> commaAnd(boolean b, Supplier<Optional<N>> nodeFactory) {
        enableAndComma.push(b);
        try {
            return nodeFactory.get();
        } finally {
            enableAndComma.pop();
        }
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> fetchNodes(String delimiter, NodeFactory<C, N, E, O> factory) {
        return new ArrayList<T>() {{
            add((T) factory.fetch(TokenParser.this));
            while (sourceCode.popWord(delimiter).isPresent())
                add((T) factory.fetch(TokenParser.this));
        }};
    }

    public Optional<N> fetchNodeAfter(String token, NodeFactory<C, N, E, O> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.fetch(this).setPositionBegin(t.getPosition()));
    }

    public Optional<ExpressionClause<C, N>> fetchNodeAfter(
            String token, ExpressionClauseFactory<C, N, E, O> clauseFactory) {
        return sourceCode.popWord(token).map(t -> clauseFactory.fetch(this).map(node ->
                node.setPositionBegin(t.getPosition())));
    }

    public Optional<N> fetchExpression(N left, OperatorMatcher<C, N, E, O> operatorMatcher,
                                       NodeFactory<C, N, E, O> rightCompiler) {
        return operatorMatcher.fetch(this).map(opt -> (OperatorFactory<C, N, E, O>) _ignore -> opt)
                .map(operatorFactory -> fetchExpression(left, operatorFactory, rightCompiler));
    }

    public N fetchExpression(N left, OperatorFactory<C, N, E, O> operatorFactory, NodeFactory<C, N, E, O> rightCompiler) {
        O operator = operatorFactory.fetch(this);
        operators.push(operator);
        try {
            return expressionConstructor.newInstance(left, operator, rightCompiler.fetch(this))
                    .adjustOperatorOrder(expressionConstructor);
        } finally {
            operators.pop();
        }
    }

    public ExpressionClause<C, N> fetchExpressionClause(OperatorFactory<C, N, E, O> operatorFactory,
                                                        NodeFactory<C, N, E, O> rightCompiler) {
        return fetchExpressionClause(operatorFactory.fetch(this), rightCompiler);
    }

    private ExpressionClause<C, N> fetchExpressionClause(O operator, NodeFactory<C, N, E, O> rightCompiler) {
        operators.push(operator);
        N right;
        try {
            right = rightCompiler.fetch(this);
        } finally {
            operators.pop();
        }
        return input -> expressionConstructor.newInstance(input, operator, right).adjustOperatorOrder(expressionConstructor);
    }

    public Optional<ExpressionClause<C, N>> fetchExpressionClause(OperatorMatcher<C, N, E, O> operatorMatcher,
                                                                  NodeFactory<C, N, E, O> rightCompiler) {
        return operatorMatcher.fetch(this).map(operator -> fetchExpressionClause(operator, rightCompiler));
    }

    public boolean isEnableCommaAnd() {
        return enableAndComma.getFirst();
    }

    public Optional<N> wordToken(String word, Function<Token, N> factory) {
        return sourceCode.popWord(word).map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }

    public <T> Optional<List<T>> fetchRow(Function<Integer, T> factory) {
        return when(sourceCode.popWord("|").isPresent()).optional(() -> new ArrayList<T>() {{
            int col = 0;
            while (!sourceCode.isEndOfLine()) {
                add(factory.apply(col++));
                sourceCode.popWord("|").orElseThrow(() -> sourceCode.syntaxError("Should end with `|`", 0));
            }
        }});
    }

    public Supplier<Optional<? extends SequenceNode>> sequenceOf(String sequenceChar, SequenceNode.Type type) {
        return () -> getSourceCode().repeatWords(sequenceChar, count -> new SequenceNode(count, type, sequenceChar));
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }

    public Optional<O> currentOperator() {
        return operators.stream().findFirst();
    }
}
