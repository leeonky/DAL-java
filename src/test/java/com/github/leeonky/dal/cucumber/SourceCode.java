package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;

import java.util.Map;
import java.util.Optional;

public class SourceCode {
    private final String code;
    private final char[] chars;
    private int position = 0;

    public SourceCode(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    Optional<Token> fetch() {
        if (Character.isDigit(currentChar())) {
            Token token = new Token(position);
            while (position < chars.length && !Constants.TOKEN_DELIMITER.contains(currentChar()))
                token.appendChar(popChar());
            return Optional.of(token);
        }
        return Optional.empty();
    }

    private char currentChar() {
        return chars[position];
    }

    public SourceCode leftTrim() {
        while (Character.isWhitespace(currentChar()))
            position++;
        return this;
    }

    public Optional<Token> fetchBetween(char c, Map<String, Character> escapes) {
        if (c == currentChar()) {
            Token token = new Token(position++);
            while (!(c == currentChar())) {
                if (!isAppendEscapeContent(escapes, token))
                    token.appendChar(popChar());
                if (position >= chars.length)
                    throw new SyntaxException(position, String.format("should end with `%c`", c));
            }
            position++;
            return Optional.of(token);
        }
        return Optional.empty();
    }

    private boolean isAppendEscapeContent(Map<String, Character> escapes, Token token) {
        return escapes.entrySet().stream().filter(e -> code.startsWith(e.getKey(), position))
                .peek(e -> {
                    token.appendChar(e.getValue());
                    position += e.getKey().length();
                }).count() != 0;
    }

    private char popChar() {
        return chars[position++];
    }
}
