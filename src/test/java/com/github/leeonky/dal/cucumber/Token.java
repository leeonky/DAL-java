package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.RegexNode;

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

    public void appendChar(char c) {
        contentBuilder.append(c);
    }

    public Node toRegex() {
        return new RegexNode(getContent()).setPositionBegin(position);
    }
}
