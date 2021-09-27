package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.Operator;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.github.leeonky.dal.util.IfThenFactory.when;
import static java.util.Arrays.asList;
import static java.util.Optional.*;
import static java.util.stream.Collectors.toList;

public class SourceCode {
    private final String code;
    private final char[] chars;
    private int position = 0;

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

    private boolean hasCode() {
        return position < chars.length;
    }

    private boolean whenFirstChar(Predicate<Character> predicate) {
        return leftTrim().hasCode() && predicate.test(currentChar());
    }

    //TODO use ifWhen
    public Optional<Token> fetchNumber() {
        if (whenFirstChar(Constants.DIGITAL_CHAR::contains)) {
            Token token = new Token(position).append(popChar());
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
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

    public Optional<Node> fetchNode(char opening, char closing,
                                    Function<Node, Node> nodeFactory, MandatoryNodeParser nodeParser, String message) {
        return fetchElements(FetchBy.BY_NODE, opening, closing, args -> {
            if (args.size() != 1)
                throw new SyntaxException(message, getPosition() - 1);
            return nodeFactory.apply(args.get(0));
        }, () -> nodeParser.fetch(this));
    }

    public <T> Optional<Node> fetchElements(FetchBy fetchBy, char opening, char closing,
                                            //TODO Supplier<T> => Function<SourceCode, T>
                                            Function<List<T>, Node> nodeFactory, Supplier<T> element) {
        if (whenFirstChar(c -> c == opening)) {
            int startPosition = position++;
            return of(nodeFactory.apply(fetchElements(fetchBy, closing, element)).setPositionBegin(startPosition));
        }
        return Optional.empty();
    }

    private <T> List<T> fetchElements(FetchBy fetchBy, char closing, Supplier<T> element) {
        List<T> elements = new ArrayList<>();
        while (hasCode() && closing != currentChar()) {
            elements.add(element.get());
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
        if (whenFirstChar(c -> '.' == c)) {
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

    private boolean startsWith(String word) {
        leftTrim();
        return (code.startsWith(word, position));
    }

    private String popWord(String word) {
        position += word.length();
        return word;
    }

    //TODO refactor
    public Optional<Token> fetchIdentityProperty() {
        if (!whenFirstChar(Constants.TOKEN_DELIMITER::contains) && hasCode()
                && !startsWith(Constants.KeyWords.IS)
                && !startsWith(Constants.KeyWords.WHICH)
                && !startsWith(Constants.KeyWords.TRUE)
                && !startsWith(Constants.KeyWords.FALSE)
                && !startsWith(Constants.KeyWords.NULL)
                && !startsWith(Constants.KeyWords.AND)
                && !startsWith(Constants.KeyWords.OR)) {
            Token token = new Token(position);
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()) && currentChar() != '.')
                token.append(popChar());
            return of(token);
        }
        return Optional.empty();
    }

    public Token fetchIdentityToken() {
        if (!whenFirstChar(Constants.TOKEN_DELIMITER::contains) && hasCode()) {
            Token token = new Token(position);
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            return token;
        }
        throw new SyntaxException(hasCode() ? "operand of `is` must be schema type"
                : "schema expression not finished", position);
    }

    public char escapedPop(EscapeChars escapeChars) {
        return escapeChars.escapeAt(code, position, length -> position += length).orElseGet(this::popChar);
    }

    public int getPosition() {
        return position;
    }

    public Optional<Operator> popUnaryOperator() {
        return popOperator(unaryOperatorFactories);
    }

    private Optional<Operator> popOperator(List<OperatorFactory> factories) {
        return factories.stream().map(OperatorFactory::popOperator).filter(Objects::nonNull).findFirst();
    }

    public Optional<Operator> popBinaryArithmeticOperator() {
        return popOperator(binaryArithmeticOperatorFactories);
    }

    public Optional<Operator> popBinaryOperator() {
        return popOperator(binaryOperatorFactories);
    }

    public enum FetchBy {
        BY_CHAR,
        BY_NODE {
            @Override
            protected void afterFetchElement(SourceCode sourceCode) {
                sourceCode.leftTrim();
            }
        };

        protected void afterFetchElement(SourceCode sourceCode) {
        }
    }

    public boolean isBeginning() {
        return IntStream.range(0, position).mapToObj(i -> chars[i]).allMatch(Character::isWhitespace);
    }

    private final List<OperatorFactory> unaryOperatorFactories = asList(
            new OperatorFactory("-", Operator.Minus::new) {
                @Override
                protected boolean matches() {
                    return super.matches() && !isBeginning();
                }
            },
            new OperatorFactory("!", Operator.Not::new));

    private final List<OperatorFactory> binaryArithmeticOperatorFactories = asList(
            new OperatorFactory("&&", () -> new Operator.And("&&")),
            new OperatorFactory("||", () -> new Operator.Or("||")),
            new OperatorFactory("and", () -> new Operator.And("and")),
            new OperatorFactory("or", () -> new Operator.Or("or")),
            new OperatorFactory(">=", Operator.GreaterOrEqual::new),
            new OperatorFactory("<=", Operator.LessOrEqual::new),
            new OperatorFactory(">", Operator.Greater::new),
            new OperatorFactory("<", Operator.Less::new),
            new OperatorFactory("+", Operator.Plus::new),
            new OperatorFactory("-", Operator.Subtraction::new),
            new OperatorFactory("*", Operator.Multiplication::new),
            new OperatorFactory("/", Operator.Division::new)
    );
    private final List<OperatorFactory> judgementOperatorFactories = asList(
            new OperatorFactory(":", Operator.Matcher::new),
            new OperatorFactory("=", Operator.Equal::new));

    private final List<OperatorFactory> binaryOperatorFactories = Stream.of(
            binaryArithmeticOperatorFactories, judgementOperatorFactories).flatMap(List::stream).collect(toList());

    public class OperatorFactory {
        private final String symbol;
        private final Supplier<Operator> factory;

        public OperatorFactory(String symbol, Supplier<Operator> factory) {
            this.symbol = symbol;
            this.factory = factory;
        }

        public Operator popOperator() {
            return when(matches()).thenReturn(() -> {
                int p = position;
                position += symbol.length();
                return factory.get().setPosition(p);
            });
        }

        protected boolean matches() {
            return startsWith(symbol);
        }
    }
}
