package com.github.leeonky.interpreter;

import com.github.leeonky.util.NumberParser;

import java.math.BigInteger;

public class Token {
    private final StringBuilder content;
    private final int position;
    private static final NumberParser NUMBER_PARSER = new NumberParser();

    public int getPosition() {
        return position;
    }

    public Token(int position) {
        this.position = position;
        content = new StringBuilder();
    }

    public Number getInteger() {
        Number number = getNumber();
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
        return content.toString();
    }

    public void append(char c) {
        content.append(c);
    }

    public Token append(String str) {
        content.append(str);
        return this;
    }

    public boolean isNumber() {
        return getNumber() != null;
    }
}
