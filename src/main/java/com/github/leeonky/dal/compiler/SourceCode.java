package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Expression;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.compiler.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.dal.compiler.SourceCode.FetchBy.BY_NODE;
import static com.github.leeonky.dal.util.IfThenFactory.when;
import static java.util.Collections.singleton;
import static java.util.Optional.*;
import static java.util.stream.Collectors.joining;

//TODO refactor methods
public class SourceCode {
    private final String code;
    private final char[] chars;
    private int position = 0;
    private final LinkedList<Operator> operators = new LinkedList<>();
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));

    public SourceCode(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    private char currentChar() {
        return chars[position];
    }

    private SourceCode leftTrim() {
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

    //TODO use ifWhen
    public Optional<Token> fetchNumber() {
        //TODO refactor
        int savePosition = position;
        if (whenFirstChar(Constants.DIGITAL_CHAR::contains)) {
            Token token = new Token(position).append(popChar());
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            if (!token.isNumber()) {
                position = savePosition;
                return empty();
            }
            return of(token);
        }
        return empty();
    }

    public Optional<Token> fetchInteger() {
        if (whenFirstChar(o -> Constants.DIGITAL_CHAR.contains(o) || o == '-')) {
            Token token = new Token(position).append(popChar());
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            return of(token);
        }
        return empty();
    }

    public Optional<Node> fetchNode(char opening, char closing, Function<Node, Node> nodeFactory,
                                    NodeCompiler nodeParser, String message) {
        return fetchNodes(opening, closing, args -> {
            if (args.size() != 1)
                throw new SyntaxException(message, getPosition() - 1);
            return nodeFactory.apply(args.get(0));
        }, i -> nodeParser.fetch(this));
    }

    public <T extends Node> Optional<Node> fetchNodes(char opening, char closing,
                                                      Function<List<T>, Node> nodeFactory, Function<Integer, T> element) {
        return when(whenFirstChar(c -> c == opening)).optional(() -> {
            int startPosition = position++;
            return nodeFactory.apply(fetchElements(BY_NODE, closing, element)).setPositionBegin(startPosition);
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

    public Optional<Token> fetchProperty() {
        if (whenFirstChar(c -> '.' == c) && !startsWith(Constants.LIST_TAIL)) {
            Token token = new Token(position++);
            leftTrim();
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()) && currentChar() != '.')
                token.append(popChar());
            if (token.contentEmpty())
                throw new SyntaxException("property is not finished", position);
            return of(token);
        }
        return Optional.empty();
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

    //TODO refactor
    public Optional<Token> fetchIdentityProperty() {
        int savePosition = position;
        if (!whenFirstChar(Constants.TOKEN_DELIMITER::contains) && hasCode()
                && !startsWith(Constants.KeyWords.IS)
                && !startsWith(Constants.KeyWords.WHICH)
                && !startsWith(Constants.KeyWords.TRUE)
                && !startsWith(Constants.KeyWords.FALSE)
                && !startsWith(Constants.KeyWords.NULL)
                && !startsWith(Constants.KeyWords.AND)
                && !startsWith(Constants.KeyWords.OR)) {
            if (fetchNumber().isPresent()) {
                position = savePosition;
                return empty();
            }
            Token token = new Token(position);
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()) && currentChar() != '.')
                token.append(popChar());
            return of(token);
        }
        return Optional.empty();
    }

    public Token fetchSchemaToken() {
        if (!whenFirstChar(Constants.TOKEN_DELIMITER::contains) && hasCode()
                && !startsWith(Constants.KeyWords.IS)
                && !startsWith(Constants.KeyWords.WHICH)
                && !startsWith(Constants.KeyWords.TRUE)
                && !startsWith(Constants.KeyWords.FALSE)
                && !startsWith(Constants.KeyWords.NULL)
                && !startsWith(Constants.KeyWords.AND)
                && !startsWith(Constants.KeyWords.OR)) {
            int savePosition = position;
            if (!fetchNumber().isPresent()) {
                Token token = new Token(position);
                while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                    token.append(popChar());
                return token;
            }
            position = savePosition;
        }
        throw new SyntaxException(hasCode() ? "operand of `is` must be schema type"
                : "schema expression is not finished", position);
    }

    public char escapedPop(EscapeChars escapeChars) {
        return escapeChars.escapeAt(code, position, length -> position += length).orElseGet(this::popChar);
    }

    public int getPosition() {
        return position;
    }

    public Optional<Node> disableCommaAnd(Supplier<Optional<Node>> nodeFactory) {
        return commaAnd(false, nodeFactory);
    }

    private Optional<Node> commaAnd(boolean b, Supplier<Optional<Node>> nodeFactory) {
        enableAndComma.push(b);
        try {
            return nodeFactory.get();
        } finally {
            enableAndComma.pop();
        }
    }

    public Optional<Node> enableCommaAnd(Supplier<Optional<Node>> nodeFactory) {
        return commaAnd(true, nodeFactory);
    }

    public Optional<Node> fetchInput() {
        return when(isBeginning()).optional(() -> InputNode.INSTANCE);
    }

    public Optional<Node> fetchString(char opening, char closing,
                                      Function<String, Node> nodeFactory, EscapeChars escapeChars) {
        return when(whenFirstChar(c -> c == opening)).optional(() -> {
            int startPosition = position++;
            return nodeFactory.apply(fetchElements(BY_CHAR, closing, i -> escapedPop(escapeChars))
                    .stream().map(String::valueOf).collect(joining(""))).setPositionBegin(startPosition);
        });
    }

    @SuppressWarnings("unchecked")
    public <T extends Node> List<T> fetchNodes(String delimiter, NodeCompiler factory) {
        return new ArrayList<T>() {{
            add((T) factory.fetch(SourceCode.this));
            while (fetchWord(delimiter).isPresent())
                add((T) factory.fetch(SourceCode.this));
        }};
    }

    public Optional<Node> fetchNodeAfter(String token, NodeCompiler nodeCompiler) {
        return fetchWord(token).map(t -> nodeCompiler.fetch(this).setPositionBegin(t.getPosition()));
    }

    public Optional<Node> fetchExpression(Node left, OperatorParser operatorParser, NodeCompiler rightParser) {
        return operatorParser.fetch(this).map(opt -> (OperatorCompiler) _ignore -> opt)
                .map(operatorCompiler -> fetchExpression(left, operatorCompiler, rightParser));
    }

    public Expression fetchExpression(Node left, OperatorCompiler operatorCompiler, NodeCompiler rightParser) {
        Operator operator = operatorCompiler.fetch(this);
        operators.push(operator);
        try {
            return new Expression(left, operator, rightParser.fetch(this)).adjustOperatorOrder();
        } finally {
            operators.pop();
        }
    }

    //    TODO refactor
    public enum FetchBy {
        BY_CHAR,
        BY_NODE {
            @Override
            protected void afterFetchElement(SourceCode sourceCode) {
                sourceCode.leftTrim();
                //TODO test: white space after last comma [1,2, ]
                sourceCode.fetchWord(",");
            }
        };

        protected void afterFetchElement(SourceCode sourceCode) {
        }
    }

    public boolean isBeginning() {
        return IntStream.range(0, position).mapToObj(i -> chars[i]).allMatch(Character::isWhitespace);
    }

    public static final OperatorCompiler DEFAULT_JUDGEMENT_OPERATOR = sourceCode -> sourceCode.operators.isEmpty() ?
            new Operator.Matcher() : sourceCode.operators.getFirst();

    public static class OperatorFactory implements OperatorParser {
        private final String symbol;
        private final Supplier<Operator> factory;

        public OperatorFactory(String symbol, Supplier<Operator> factory) {
            this.symbol = symbol;
            this.factory = factory;
        }

        @Override
        public Optional<Operator> fetch(SourceCode sourceCode) {
            return when(matches(sourceCode)).optional(() -> {
                int p = sourceCode.position;
                sourceCode.position += symbol.length();
                return factory.get().setPosition(p);
            });
        }

        protected boolean matches(SourceCode sourceCode) {
            return sourceCode.startsWith(symbol);
        }

        public boolean isEnableAndComma(SourceCode sourceCode) {
            return sourceCode.enableAndComma.getFirst();
        }
    }
}
