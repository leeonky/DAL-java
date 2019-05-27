package com.github.leeonky.dal.token;

import java.util.List;
import java.util.Objects;

import static java.util.Arrays.asList;

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

    public static Token rootValueToken() {
        return new Token(Type.ROOT_VALUE, null);
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

    public List<String> getProperties() {
        return (List<String>) value;
    }

    boolean isKeyWord(String keyword) {
        return type == Type.KEY_WORD && value.equals(keyword);
    }

    public Type getType() {
        return type;
    }

    public enum Type {
        WORD, PROPERTY, CONST_INDEX, OPERATOR, BEGIN_BRACKET, END_BRACKET, KEY_WORD, ROOT_VALUE, CONST_VALUE
    }
}
