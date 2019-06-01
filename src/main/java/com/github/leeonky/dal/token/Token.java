package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Operator;

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

    public Operator toOperator() {
        String operatorString = value.toString();
        Operator operator;
        switch (operatorString) {
            case "=":
                operator = new Operator.Equal();
                break;
            case ">":
                operator = new Operator.Greater();
                break;
            case "<":
                operator = new Operator.Less();
                break;
            case ">=":
                operator = new Operator.GreaterOrEqual();
                break;
            case "<=":
                operator = new Operator.LessOrEqual();
                break;
            case "!=":
                operator = new Operator.NotEqual();
                break;
            case "+":
                operator = new Operator.Plus();
                break;
            case "-":
                operator = new Operator.Subtraction();
                break;
            case "*":
                operator = new Operator.Multiplication();
                break;
            case "/":
                operator = new Operator.Division();
                break;
            case "&&":
                operator = new Operator.And();
                break;
            case "||":
                operator = new Operator.Or();
                break;
            default:
                throw new SyntaxException(getPositionBegin(), "not support operator " + operatorString + " yet");
        }
        operator.setPosition(getPositionBegin());
        return operator;
    }

    public enum Type {
        WORD, PROPERTY, CONST_INDEX, OPERATOR, BEGIN_BRACKET, END_BRACKET, KEY_WORD, CONST_VALUE
    }
}
