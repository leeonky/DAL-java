package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;

import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;

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

    private boolean isFirstChar(Predicate<Character> predicate) {
        return leftTrim().hasCode() && predicate.test(currentChar());
    }

    public Optional<Token> fetch() {
        if (isFirstChar(Constants.DIGITAL_CHAR::contains)) {
            Token token = new Token(position);
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            return of(token);
        }
        return empty();
    }

    public Optional<Token> fetchBetween(Character c, Map<String, Character> escapes) {
        if (isFirstChar(c::equals)) {
            Token token = new Token(position++);
            while (!(c == currentChar())) {
                if (!isAppendEscapeContent(escapes, token))
                    token.append(popChar());
                if (position >= chars.length)
                    throw new SyntaxException(position, String.format("should end with `%c`", c));
            }
            position++;
            return of(token);
        }
        return Optional.empty();
    }

    private boolean isAppendEscapeContent(Map<String, Character> escapes, Token token) {
        return escapes.entrySet().stream().filter(e -> code.startsWith(e.getKey(), position))
                .peek(e -> {
                    token.append(e.getValue());
                    position += e.getKey().length();
                }).count() != 0;
    }

    private char popChar() {
        return chars[position++];
    }

    public Optional<Token> fetchProperty() {
        if (isFirstChar(c -> '.' == c)) {
            Token token = new Token(position++);
            leftTrim();
            while (hasCode() && !Constants.TOKEN_DELIMITER.contains(currentChar()) && currentChar() != '.')
                token.append(popChar());
            if (token.contentEmpty())
                throw new SyntaxException(position, "property is not finished");
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
        if (!isFirstChar(Constants.TOKEN_DELIMITER::contains) && hasCode()
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
}
