package com.github.leeonky.interpreter;

import java.math.BigInteger;

public class Token {
    private final StringBuilder contentBuilder;
    private final int position;
    private static final NumberParser NUMBER_PARSER = new NumberParser();

    public int getPosition() {
        return position;
    }

    public Token(int position) {
        this.position = position;
        contentBuilder = new StringBuilder();
    }

    public Number getInteger() {
        String content = getContent();
        Number number = NUMBER_PARSER.parse(content);
        if (number != null) {
            Class<? extends Number> type = number.getClass();
            if (type.equals(Integer.class) || type.equals(Long.class) || type.equals(Short.class)
                    || type.equals(Byte.class) || type.equals(BigInteger.class)) {
                return number;
            }
        }
        throw new SyntaxException("expect an integer", position);
    }

    public Number getNumber() {
        return NUMBER_PARSER.parse(getContent());
    }

    public String getContent() {
        return contentBuilder.toString();
    }

    public Token append(char c) {
        contentBuilder.append(c);
        return this;
    }

    public Token append(String str) {
        contentBuilder.append(str);
        return this;
    }

    public boolean isNumber() {
        return getNumber() != null;
    }
}
