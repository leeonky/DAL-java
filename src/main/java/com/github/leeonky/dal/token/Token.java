package com.github.leeonky.dal.token;

import java.math.BigDecimal;
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

    public static Token constIndexToken(int value) {
        return new Token(Type.CONST_INDEX, value);
    }

    public static Token operatorToken(String value) {
        return new Token(Type.OPERATOR, value);
    }

    public static Token numberToken(Object value) {
        return new Token(Type.NUMBER, value);
    }

    public static Token beginBracketToken() {
        return new Token(Type.BEGIN_BRACKET, "(");
    }

    public static Token stringToken(String value) {
        return new Token(Type.STRING, value);
    }

    public static Token endBracketToken() {
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
        return type + "<" + value + ">";
    }

    public String getWord() {
        return value.toString();
    }

    public String getOperator() {
        return value.toString();
    }

    public BigDecimal getNumber() {
        return (BigDecimal) value;
    }

    boolean isWord(String keyword) {
        return type == Type.WORD && value.equals(keyword);
    }

    enum Type {
        NUMBER, WORD, PROPERTY, CONST_INDEX, OPERATOR, BEGIN_BRACKET, END_BRACKET, STRING
    }
}
