package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.Operator;

import java.math.BigDecimal;
import java.util.Objects;

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

    public static Token propertyToken(Object property) {
        return new Token(Type.PROPERTY, property);
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

    //TODO support for opt '=' regex matches regex without target value to string
    public static Token regexToken(String regex) {
        return new Token(Type.REGEX, regex);
    }

    public static Token endBracketToken() {
        return new Token(Type.END_BRACKET, ")");
    }

    public static Token keyWordToken(String keyWord) {
        return new Token(Type.KEY_WORD, keyWord);
    }

    public static Token treeToken(TokenStream tokenStream) {
        return new Token(Type.TREE, tokenStream);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Token
                && Objects.equals(type, ((Token) obj).type)
                && Objects.equals(value, ((Token) obj).value);
    }

    public Object getValue() {
        return value;
    }

    public Type getType() {
        return type;
    }

    public int getPositionEnd() {
        return positionEnd;
    }

    public Token setPositionEnd(int positionEnd) {
        this.positionEnd = positionEnd;
        return this;
    }

    public int getPositionBegin() {
        return positionBegin;
    }

    public Token setPositionBegin(int positionBegin) {
        this.positionBegin = positionBegin;
        return this;
    }

    public Operator toOperator(boolean isUnaryOperator) {
        String operatorString = value.toString();
        Operator operator;
        if (isUnaryOperator) {
            switch (operatorString) {
                case "-":
                    operator = new Operator.Minus();
                    break;
                case "!":
                    operator = new Operator.Not();
                    break;
                default:
                    throw new SyntaxException(getPositionBegin(), "not support operator " + operatorString + " yet");
            }
        } else {
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
                    operator = new Operator.And("&&");
                    break;
                case "||":
                    operator = new Operator.Or();
                    break;
                case Constants.Operators.MATCH:
                    operator = new Operator.Matcher();
                    break;
                case ",":
                    operator = new Operator.Comma();
                    break;
                default:
                    throw new SyntaxException(getPositionBegin(), "not support operator `" + operatorString + "` yet");
            }
        }
        operator.setPosition(getPositionBegin());
        return operator;
    }

    public boolean judgement() {
        return getType() == Type.OPERATOR
                && (getValue().equals(Constants.Operators.MATCH) || getValue().equals(Constants.Operators.EQ));
    }

    public Object getPropertyOrIndex() {
        if (value instanceof BigDecimal) {
            try {
                return Integer.valueOf(value.toString());
            } catch (NumberFormatException ignore) {
                throw new SyntaxException(getPositionBegin(), "must be integer");
            }
        }
        return getValue();
    }

    public TokenStream getTokenStream() {
        return (TokenStream) value;
    }

    public enum Type {
        WORD, PROPERTY, OPERATOR, BEGIN_BRACKET, END_BRACKET, KEY_WORD, CONST_VALUE, REGEX, TREE
    }
}
