package com.github.leeonky.dal.token;

import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

public class Token {
    private final Type type;
    private final Object value;
    private int positionBegin, positionEnd;

    public Token(Type type, Object value) {
        this.type = type;
        this.value = value;
    }

    public static Token wordToken(Object value) {
        return new Token(Type.WORD, value);
    }

    public static Token propertyToken(String... properties) {
        return new Token(Type.PROPERTY, asList(properties));
    }

    public static Token constIndexToken(int value) {
        return new Token(Type.CONST_INDEX, value);
    }

    public static Token operatorToken(String value) {
        return new Token(Type.OPERATOR, value);
    }

    public static Token constValueToken(Object value) {
        return new Token(Type.CONST_VALUE, value);
    }

    public static Token beginBracketToken() {
        return new Token(Type.BEGIN_BRACKET, "(");
    }

    public static Token endBracketToken() {
        return new Token(Type.END_BRACKET, ")");
    }

    public static Token keyWordToken(String keyWord) {
        return new Token(Type.KEY_WORD, keyWord);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Token
                && Objects.equals(type, ((Token) obj).type)
                && Objects.equals(value, ((Token) obj).value);
    }

    @Override
    public String toString() {
        return type + "`" + value + "`";
    }

    public Object getConstValue() {
        return value;
    }

    public String getOperator() {
        return value.toString();
    }

    public List<String> getProperties() {
        return (List<String>) value;
    }

    public Type getType() {
        return type;
    }

    public int getPositionEnd() {
        return positionEnd;
    }

    public void setPositionEnd(int positionEnd) {
        this.positionEnd = positionEnd;
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public void setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
    }

    public enum Type {
        WORD, PROPERTY, CONST_INDEX, OPERATOR, BEGIN_BRACKET, END_BRACKET, KEY_WORD, CONST_VALUE
    }
}
