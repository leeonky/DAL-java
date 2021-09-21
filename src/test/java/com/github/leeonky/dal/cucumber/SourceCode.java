package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;

import java.util.Map;
import java.util.Optional;

import static java.util.Optional.*;

public class SourceCode {
    private final String code;
    private final char[] chars;
    private int position = 0;

    public SourceCode(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    Optional<Token> fetch() {
        if (Character.isDigit(firstChar())) {
            Token token = new Token(position);
            while (position < chars.length && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            return of(token);
        }
        return empty();
    }

    private char firstChar() {
        return leftTrim().currentChar();
    }

    private char currentChar() {
        return chars[position];
    }

    private SourceCode leftTrim() {
        while (position < chars.length && Character.isWhitespace(currentChar()))
            position++;
        return this;
    }

    public Optional<Token> fetchBetween(char c, Map<String, Character> escapes) {
        if (c == firstChar()) {
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
        if ('.' == firstChar()) {
            Token token = new Token(position++);
            leftTrim();
            while (position < chars.length && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.append(popChar());
            if (token.contentEmpty())
                throw new SyntaxException(position, "property is not finished");
            return of(token);
        }
        return Optional.empty();
    }

    public Optional<Token> fetchWord(String word) {
        return ofNullable(firstStartsWith(word) ? new Token(position).append(popWord(word)) : null);
    }

    private boolean firstStartsWith(String word) {
        leftTrim();
        return (code.startsWith(word, position));
    }

    private String popWord(String word) {
        position += word.length();
        return word;
    }
}
