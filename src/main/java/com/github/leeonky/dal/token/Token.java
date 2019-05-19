package com.github.leeonky.dal.token;

import java.util.Objects;

public class Token {
    private final Type type;
    private final Object value;

    public Token(Type type, Object value) {
        this.type = type;
        this.value = value;
    }

    public static Token token(Object value) {
        return new Token(Type.TOKEN, value);
    }

    public static Token constValue(Object value) {
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
        return String.valueOf(value);
    }

    enum Type {
        VALUE, TOKEN
    }
}
