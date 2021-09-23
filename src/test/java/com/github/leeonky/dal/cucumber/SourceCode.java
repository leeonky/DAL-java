package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Node;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static java.util.Optional.*;

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

    public Optional<Token> fetchNumber() {
        if (whenFirstChar(o -> Constants.DIGITAL_CHAR.contains(o) || o == '-')) {
            Token token = new Token(position).append(popChar());
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            return of(token);
        }
        return empty();
    }

    public <T> Optional<Node> fetchElements(FetchBy fetchBy, char opening, char closing,
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

    public Optional<Token> fetchIdentity() {
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

    public char escapedPop(EscapeChars escapeChars) {
        return escapeChars.escapeAt(code, position, length -> position += length).orElseGet(this::popChar);
    }

    public int getPosition() {
        return position;
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
}
