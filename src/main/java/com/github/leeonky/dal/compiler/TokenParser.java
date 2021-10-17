package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.SourceCode.tokenMatcher;
import static com.github.leeonky.dal.compiler.TokenParser.FetchBy.BY_CHAR;
import static com.github.leeonky.dal.compiler.TokenParser.FetchBy.BY_NODE;
import static com.github.leeonky.dal.runtime.FunctionUtil.not;
import static com.github.leeonky.dal.runtime.IfThenFactory.when;
import static java.util.Collections.*;
import static java.util.stream.Collectors.joining;

public class TokenParser {
    public static final TokenMatcher
            NUMBER = tokenMatcher(DIGITAL::contains, emptyList(), false, (c1, c2) ->
            ((c1 != 'e' && c1 != 'E') || (c2 != '-' && c2 != '+')) && DELIMITER.contains(c2), Token::isNumber),
            INTEGER = tokenMatcher(DIGITAL_OR_MINUS::contains, emptyList(), false, DELIMITER, Token::isNumber),
            IDENTITY_PROPERTY = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT,
                    not(Token::isNumber)),
            DOT_PROPERTY = tokenMatcher(DOT::equals, singletonList(LIST_ELLIPSIS), true, DELIMITER_OR_DOT, Token::all);

    public static final TokenFactory SCHEMA = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).or("expect a schema");

    private final SourceCode sourceCode;
    private final LinkedList<Operator> operators = new LinkedList<>();
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));

    public TokenParser(SourceCode sourceCode) {
        this.sourceCode = sourceCode;
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }

    public Optional<Node> fetchNode(char opening, char closing, Function<Node, Node> nodeFactory,
                                    NodeFactory nodeMatcher, String message) {
        return fetchNodes(opening, closing, args -> {
            if (args.size() != 1)
                throw sourceCode.syntaxError(message, -1);
            return nodeFactory.apply(args.get(0));
        }, i -> nodeMatcher.fetch(this));
    }

    public <T extends Node> Optional<Node> fetchNodes(Character opening, char closing, Function<List<T>,
            Node> nodeFactory, Function<Integer, T> element) {
        return sourceCode.fetchElements(BY_NODE, opening, closing, element, nodeFactory);
    }

    public Optional<Node> fetchString(Character opening, char closing, Function<String, Node> nodeFactory,
                                      Map<String, Character> escapeChars) {
        return sourceCode.fetchElements(BY_CHAR, opening, closing, i -> sourceCode.escapedPop(escapeChars),
                chars -> nodeFactory.apply(chars.stream().map(String::valueOf).collect(joining())));
    }

    public Optional<Node> disableCommaAnd(Supplier<Optional<Node>> nodeFactory) {
        return commaAnd(false, nodeFactory);
    }

    public Optional<Node> enableCommaAnd(Supplier<Optional<Node>> nodeFactory) {
        return commaAnd(true, nodeFactory);
    }

    private Optional<Node> commaAnd(boolean b, Supplier<Optional<Node>> nodeFactory) {
        enableAndComma.push(b);
        try {
            return nodeFactory.get();
        } finally {
            enableAndComma.pop();
        }
    }

    public Optional<Node> fetchInput() {
        return when(sourceCode.isBeginning()).optional(() -> InputNode.INSTANCE);
    }

    @SuppressWarnings("unchecked")
    public <T extends Node> List<T> fetchNodes(String delimiter, NodeFactory factory) {
        return new ArrayList<T>() {{
            add((T) factory.fetch(TokenParser.this));
            while (sourceCode.popWord(delimiter).isPresent())
                add((T) factory.fetch(TokenParser.this));
        }};
    }

    public Optional<Node> fetchNodeAfter(String token, NodeFactory nodeFactory) {
        return sourceCode.popWord(token).map(t -> nodeFactory.fetch(this).setPositionBegin(t.getPosition()));
    }

    public Optional<Node> fetchExpression(Node left, OperatorMatcher operatorMatcher, NodeFactory rightCompiler) {
        return operatorMatcher.fetch(this).map(opt -> (OperatorFactory) _ignore -> opt)
                .map(operatorFactory -> fetchExpression(left, operatorFactory, rightCompiler));
    }

    public Expression fetchExpression(Node left, OperatorFactory operatorFactory, NodeFactory rightCompiler) {
        Operator operator = operatorFactory.fetch(this);
        operators.push(operator);
        try {
            return new Expression(left, operator, rightCompiler.fetch(this)).adjustOperatorOrder();
        } finally {
            operators.pop();
        }
    }

    public boolean isEnableCommaAnd() {
        return enableAndComma.getFirst();
    }

    public Optional<Node> wordToken(String word, Function<Token, Node> factory) {
        return sourceCode.popWord(word).map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }

    public enum FetchBy {
        BY_CHAR,
        BY_NODE {
            @Override
            protected void afterFetchElement(SourceCode sourceCode) {
                sourceCode.popWord(",");
                sourceCode.leftTrim();
            }
        };

        protected void afterFetchElement(SourceCode tokenParser) {
        }
    }

    public final OperatorFactory DEFAULT_JUDGEMENT_OPERATOR = tokenParser -> operators.isEmpty() ?
            new Operator.Matcher() : operators.getFirst();

    public static OperatorMatcher operatorMatcher(String symbol, Supplier<Operator> factory,
                                                  Predicate<TokenParser> matcher) {
        return tokenParser -> tokenParser.getSourceCode().popWord(symbol, () -> matcher.test(tokenParser))
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    public static OperatorMatcher operatorMatcher(String symbol, Supplier<Operator> factory) {
        return operatorMatcher(symbol, factory, s -> true);
    }
}
