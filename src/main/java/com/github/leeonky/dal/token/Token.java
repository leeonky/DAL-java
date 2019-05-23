package com.github.leeonky.dal.token;

import java.util.Objects;

public class Token {
    private final Type type;
    private final Object value;

    public Token(Type type, Object value) {
        this.type = type;
        this.value = value;
    }

    public static Token typeToken(Object value) {
        return new Token(Type.TYPE, value);
    }

    public static Token propertyToken(String value) {
        return new Token(Type.PROPERTY, value);
    }

    public static Token constValueToken(Object value) {
        return new Token(Type.VALUE, value);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Token
                && Objects.equals(type, ((Token) obj).type)
                && Objects.equals(value, ((Token) obj).value);
    }

    @Override
    public String toString() {
        return type + "<" + String.valueOf(value) + ">";
    }

    enum Type {
        VALUE, TYPE, PROPERTY
    }
}
