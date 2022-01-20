package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.SequenceNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
import static com.github.leeonky.interpreter.SourceCode.tokenMatcher;
import static java.util.Arrays.asList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.stream.Collectors.joining;

public class TokenParser<N extends Node<N, C>, C extends RuntimeContext<C>> {
    public static final TokenMatcher
            NUMBER = tokenMatcher(DIGITAL::contains, emptySet(), false, (lastChar, nextChar) ->
            ((lastChar != 'e' && lastChar != 'E') || (nextChar != '-' && nextChar != '+')) && DELIMITER.contains(nextChar), Token::isNumber),
            INTEGER = tokenMatcher(DIGITAL_OR_MINUS::contains, emptySet(), false, DELIMITER, Token::isNumber),
            IDENTITY_PROPERTY = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_PROPERTY = tokenMatcher(DOT::equals, new HashSet<>(asList(ELEMENT_ELLIPSIS, DOT.toString())), true, DELIMITER_OR_DOT, Token::all);

    public static final TokenFactory SCHEMA = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).or("expect a schema");

    private final SourceCode sourceCode;
    private final C runtimeContext;
    private final LinkedList<Operator<N, C>> operators = new LinkedList<>();
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));
    private final ExpressionConstructor<N, C> expressionConstructor;

    public TokenParser(SourceCode sourceCode, C runtimeContext, ExpressionConstructor<N, C> expressionConstructor) {
        this.sourceCode = sourceCode;
        this.runtimeContext = runtimeContext;
        this.expressionConstructor = expressionConstructor;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public Optional<N> fetchNode(char opening, char closing, Function<N, N> nodeFactory,
                                 NodeFactory<N, C> nodeMatcher, String message) {
        return fetchNodes(opening, closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        }, () -> nodeMatcher.fetch(this));
    }

    public <T> Optional<N> fetchNodes(Character opening, char closing, Function<List<T>, N> nodeFactory, Supplier<T> element) {
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

    public Optional<N> fetchBetween(String opening, String closing, NodeMatcher<N, C> nodeMatcher) {
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
    public <T> List<T> fetchNodes(String delimiter, NodeFactory<N, C> factory) {
        return new ArrayList<T>() {{
            add((T) factory.fetch(TokenParser.this));
            while (sourceCode.popWord(delimiter).isPresent())
                add((T) factory.fetch(TokenParser.this));
        }};
    }

    public Optional<N> fetchNodeAfter(String token, NodeFactory<N, C> nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.fetch(this).setPositionBegin(t.getPosition()));
    }

    public Optional<ExpressionClause<N, C>> fetchNodeAfter(String token, ExpressionClauseFactory<N, C> expressionClauseFactory) {
        return sourceCode.popWord(token).map(t -> expressionClauseFactory.fetch(this)
                .map(node -> node.setPositionBegin(t.getPosition())));
    }

    public Optional<N> fetchExpression(N left, OperatorMatcher<N, C> operatorMatcher, NodeFactory<N, C> rightCompiler) {
        return operatorMatcher.fetch(this).map(opt -> (OperatorFactory<N, C>) _ignore -> opt)
                .map(operatorFactory -> fetchExpression(left, operatorFactory, rightCompiler));
    }

    public N fetchExpression(N left, OperatorFactory<N, C> operatorFactory, NodeFactory<N, C> rightCompiler) {
        Operator<N, C> operator = operatorFactory.fetch(this);
        operators.push(operator);
        try {
            return expressionConstructor.newInstance(left, operator, rightCompiler.fetch(this)).adjustOperatorOrder();
        } finally {
            operators.pop();
        }
    }

    public ExpressionClause<N, C> fetchExpressionClause(OperatorFactory<N, C> operatorFactory, NodeFactory<N, C> rightCompiler) {
        return fetchExpressionClause(operatorFactory.fetch(this), rightCompiler);
    }

    private ExpressionClause<N, C> fetchExpressionClause(Operator<N, C> operator, NodeFactory<N, C> rightCompiler) {
        operators.push(operator);
        N right;
        try {
            right = rightCompiler.fetch(this);
        } finally {
            operators.pop();
        }
        return input -> expressionConstructor.newInstance(input, operator, right).adjustOperatorOrder();
    }

    public Optional<ExpressionClause<N, C>> fetchExpressionClause(OperatorMatcher<N, C> operatorMatcher, NodeFactory<N, C> rightCompiler) {
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

    public static final OperatorFactory<DALNode, DALRuntimeContext> DEFAULT_JUDGEMENT_OPERATOR
            = tokenParser -> tokenParser.operators.isEmpty() ? new Operator.Matcher() : tokenParser.operators.getFirst();

    public static OperatorMatcher<DALNode, DALRuntimeContext> operatorMatcher(
            String symbol, Supplier<Operator<DALNode, DALRuntimeContext>> factory,
            Predicate<TokenParser<DALNode, DALRuntimeContext>> matcher) {
        return tokenParser -> tokenParser.getSourceCode().popWord(symbol, () -> matcher.test(tokenParser))
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    public static OperatorMatcher<DALNode, DALRuntimeContext> operatorMatcher(
            String symbol, Supplier<Operator<DALNode, DALRuntimeContext>> factory) {
        return operatorMatcher(symbol, factory, s -> true);
    }

    public Supplier<Optional<? extends SequenceNode>> sequenceOf(String sequenceChar, SequenceNode.Type type) {
        return () -> getSourceCode().repeatWords(sequenceChar, count -> new SequenceNode(count, type, sequenceChar));
    }

    public C getRuntimeContext() {
        return runtimeContext;
    }
}
