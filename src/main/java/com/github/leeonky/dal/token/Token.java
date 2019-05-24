package com.github.leeonky.dal.token;

import java.util.Objects;

public class Token {
    private final Type type;
    private final Object value;

    public Token(Type type, Object value) {
        this.type = type;
        this.value = value;
    }

    public static Token wordToken(Object value) {
        return new Token(Type.WORD, value);
    }

    public static Token propertyToken(String value) {
        return new Token(Type.PROPERTY, value);
    }

    public static Token itemToken(String value) {
        return new Token(Type.ITEM, value);
    }

    public static Token operatorToken(String value) {
        return new Token(Type.OPERATOR, value);
    }

    public static Token numebrToken(Object value) {
        return new Token(Type.NUMBER, value);
    }

    public static Token beginBrachetToken() {
        return new Token(Type.BEGIN_BRACKET, "(");
    }

    public static Token stringToken(String value) {
        return new Token(Type.STRING, value);
    }

    public static Token endBrachetToken() {
        return new Token(Type.END_BRACKET, ")");
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
        NUMBER, WORD, PROPERTY, ITEM, OPERATOR, BEGIN_BRACKET, END_BRACKET, STRING
    }
}
