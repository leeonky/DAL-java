package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.Constants;

import java.util.Optional;

public class SourceCode {
    final String code;
    private final char[] chars;
    private int position = 0;

    public SourceCode(String code) {
        this.code = code;
        chars = code.toCharArray();
    }

    Optional<Token> fetch() {
        if (Character.isDigit(chars[position])) {
            int startPosition = position;
            while (position < chars.length && !Constants.TOKEN_DELIMITER.contains(chars[position]))
                position++;
            return Optional.of(new Token(code.substring(startPosition, position), startPosition));
        }
        return Optional.empty();
    }

    public SourceCode leftTrim() {
        while (Character.isWhitespace(chars[position]))
            position++;
        return this;
    }
}
