package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.*;

import java.math.BigDecimal;

public class Token {
    private final StringBuilder contentBuilder;
    private final int position;

    public Token(int position) {
        this.position = position;
        contentBuilder = new StringBuilder();
    }

    public Token(String content, int position) {
        this.position = position;
        contentBuilder = new StringBuilder(content);
    }

    private static Number getNumber(String content) {
        try {
            return BigDecimal.valueOf(Long.decode(content));
        } catch (NumberFormatException e) {
            return new BigDecimal(content);
        }
    }

    public Node toConstNumber() {
        return new ConstNode(getNumber(getContent())).setPositionBegin(position);
    }

    private String getContent() {
        return contentBuilder.toString();
    }

    public Node toConstString() {
        return new ConstNode(getContent()).setPositionBegin(position);
    }

    public void append(char c) {
        contentBuilder.append(c);
    }

    public Token append(String str) {
        contentBuilder.append(str);
        return this;
    }

    public Node toRegex() {
        return new RegexNode(getContent()).setPositionBegin(position);
    }

    public Node toDotProperty() {
        return new PropertyNode(InputNode.INSTANCE, getContent(), PropertyNode.Type.DOT).setPositionBegin(position);
    }

    public boolean contentEmpty() {
        return contentBuilder.length() == 0;
    }

    public Node toConstTrue() {
        return new ConstNode(true).setPositionBegin(position);
    }

    public Node toConstFalse() {
        return new ConstNode(false).setPositionBegin(position);
    }

    public Node toConstNull() {
        return new ConstNode(null).setPositionBegin(position);
    }
}
