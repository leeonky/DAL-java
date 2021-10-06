package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.TokenParser.FetchBy.BY_CHAR;
import static com.github.leeonky.dal.compiler.TokenParser.FetchBy.BY_NODE;
import static com.github.leeonky.dal.runtime.Function.not;
import static com.github.leeonky.dal.runtime.IfThenFactory.when;
import static java.util.Collections.*;
import static java.util.Optional.*;
import static java.util.stream.Collectors.joining;

//TODO refactor methods
public class TokenParser {
    public static final TokenMatcher
            NUMBER = tokenMatcher(DIGITAL::contains, emptyList(), false, DELIMITER, Token::isNumber),
            INTEGER = tokenMatcher(DIGITAL_OR_MINUS::contains, emptyList(), false, DELIMITER, Token::isNumber),
            IDENTITY_PROPERTY = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT,
                    not(Token::isNumber)),
            DOT_PROPERTY = tokenMatcher(DOT::equals, singletonList(LIST_TAIL), true, DELIMITER_OR_DOT, Token::all);

    public static final TokenFactory SCHEMA = tokenMatcher(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).or("expect a schema");

    private final String code;
    private final char[] chars;
    private int position = 0;
    private final LinkedList<Operator> operators = new LinkedList<>();
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));

    public TokenParser(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    public static TokenMatcher tokenMatcher(Predicate<Character> startsWith, Collection<String> excluded,
                                            boolean trim, Set<Character> delimiters, Predicate<Token> validator) {
        return parser -> {
            if (parser.whenFirstChar(startsWith) && parser.hasCode()
                    && excluded.stream().noneMatch(parser::startsWith)) {
                Token token = new Token(parser.position);
                if (trim) {
                    parser.position++;
                    parser.leftTrim();
                }
                if (parser.hasCode())
                    do token.append(parser.popChar());
                    while (parser.hasCode() && !delimiters.contains(parser.currentChar()));
                if (validator.test(token))
                    return of(token);
                parser.position = token.getPosition();
            }
            return empty();
        };
    }

    private char currentChar() {
        return chars[position];
    }

    private TokenParser leftTrim() {
        while (hasCode() && Character.isWhitespace(currentChar()))
            position++;
        return this;
    }

    public boolean hasCode() {
        return position < chars.length;
    }

    private boolean whenFirstChar(Predicate<Character> predicate) {
        return leftTrim().hasCode() && predicate.test(currentChar());
    }

    public Optional<Node> fetchNode(char opening, char closing, Function<Node, Node> nodeFactory,
                                    NodeFactory nodeMatcher, String message) {
        return fetchNodes(opening, closing, args -> {
            if (args.size() != 1)
                throw new SyntaxException(message, getPosition() - 1);
            return nodeFactory.apply(args.get(0));
        }, i -> nodeMatcher.fetch(this));
    }

    public <T extends Node> Optional<Node> fetchNodes(char opening, char closing, Function<List<T>, Node> nodeFactory,
                                                      Function<Integer, T> element) {
        return when(whenFirstChar(c -> c == opening)).optional(() -> {
            int startPosition = position++;
            return nodeFactory.apply(fetchElements(BY_NODE, closing, element)).setPositionBegin(startPosition);
        });
    }

    public Optional<Node> fetchString(char opening, char closing, Function<String, Node> nodeFactory,
                                      EscapeChars escapeChars) {
        return when(whenFirstChar(c -> c == opening)).optional(() -> {
            int startPosition = position++;
            return nodeFactory.apply(fetchElements(BY_CHAR, closing, i -> escapedPop(escapeChars))
                    .stream().map(String::valueOf).collect(joining(""))).setPositionBegin(startPosition);
        });
    }

    private <T> List<T> fetchElements(FetchBy fetchBy, char closing, Function<Integer, T> element) {
        List<T> elements = new ArrayList<>();
        int index = 0;
        while (hasCode() && closing != currentChar()) {
            elements.add(element.apply(index++));
            fetchBy.afterFetchElement(this);
        }
        if (position >= chars.length)
            throw new SyntaxException(String.format("should end with `%c`", closing), position);
        position++;
        return elements;
    }

    private char popChar() {
        return chars[position++];
    }

    public Optional<Token> fetchWord(String word) {
        return ofNullable(startsWith(word) ? new Token(position).append(popWord(word)) : null);
    }

    public boolean startsWith(String word) {
        leftTrim();
        return (code.startsWith(word, position));
    }

    private String popWord(String word) {
        position += word.length();
        return word;
    }

    private char escapedPop(EscapeChars escapeChars) {
        return escapeChars.escapeAt(code, position, length -> position += length).orElseGet(this::popChar);
    }

    public int getPosition() {
        return position;
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
        return when(isBeginning()).optional(() -> InputNode.INSTANCE);
    }

    @SuppressWarnings("unchecked")
    public <T extends Node> List<T> fetchNodes(String delimiter, NodeFactory factory) {
        return new ArrayList<T>() {{
            add((T) factory.fetch(TokenParser.this));
            while (fetchWord(delimiter).isPresent())
                add((T) factory.fetch(TokenParser.this));
        }};
    }

    public Optional<Node> fetchNodeAfter(String token, NodeFactory nodeFactory) {
        return fetchWord(token).map(t -> nodeFactory.fetch(this).setPositionBegin(t.getPosition()));
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

    public boolean isEnableAndComma() {
        return enableAndComma.getFirst();
    }

    public enum FetchBy {
        BY_CHAR,
        BY_NODE {
            @Override
            protected void afterFetchElement(TokenParser tokenParser) {
                tokenParser.fetchWord(",");
                tokenParser.leftTrim();
            }
        };

        protected void afterFetchElement(TokenParser tokenParser) {
        }
    }

    public boolean isBeginning() {
        return IntStream.range(0, position).mapToObj(i -> chars[i]).allMatch(Character::isWhitespace);
    }

    public static final OperatorFactory DEFAULT_JUDGEMENT_OPERATOR = parser -> parser.operators.isEmpty() ?
            new Operator.Matcher() : parser.operators.getFirst();

    public static OperatorMatcher operatorMatcher(String symbol, Supplier<Operator> factory,
                                                  Predicate<TokenParser> matcher) {
        return parser -> when(parser.startsWith(symbol) && matcher.test(parser)).optional(() -> {
            int p = parser.position;
            parser.position += symbol.length();
            return factory.get().setPosition(p);
        });
    }

    public static OperatorMatcher operatorMatcher(String symbol, Supplier<Operator> factory) {
        return operatorMatcher(symbol, factory, s -> true);
    }
}
