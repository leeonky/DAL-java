package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;

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

    public Optional<Token> fetchSingleQuotedString() {
        if ('\'' == currentChar()) {
            Token token = new Token(position);
            position++;
            while ('\'' != currentChar()) {
                if (currentChar() == '\\') {
                    if (code.startsWith("\\\\", position)) {
                        token.appendChar('\\');
                        position += 2;
                    } else if (code.startsWith("\\'", position)) {
                        token.appendChar('\'');
                        position += 2;
                    } else token.appendChar(popChar());
                } else
                    token.appendChar(popChar());
                if (position >= chars.length)
                    throw new SyntaxException(position, "should end with `'`");
            }
            position++;
            return Optional.of(token);
        }
        return Optional.empty();
    }

    private char popChar() {
        return chars[position++];
    }

    public Optional<Token> fetchDoubleQuotedString() {
        if ('"' == currentChar()) {
            Token token = new Token(position);
            position++;
            while ('"' != currentChar()) {
                if (currentChar() == '\\') {
                    if (code.startsWith("\\\\", position)) {
                        token.appendChar('\\');
                        position += 2;
                    } else if (code.startsWith("\\'", position)) {
                        token.appendChar('\'');
                        position += 2;
                    } else token.appendChar(popChar());
                } else
                    token.appendChar(popChar());
                if (position >= chars.length)
                    throw new SyntaxException(position, "should end with `'`");
            }
            position++;
            return Optional.of(token);
        }
        return Optional.empty();
    }
}
