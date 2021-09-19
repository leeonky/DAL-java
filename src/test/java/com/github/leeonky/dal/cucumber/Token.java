package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;

import java.math.BigDecimal;

public class Token {
    private final String content;
    private final int position;

    public Token(String content, int position) {
        this.content = content;
        this.position = position;
    }

    private static Number getNumber(String content) {
        try {
            return BigDecimal.valueOf(Long.decode(content));
        } catch (NumberFormatException e) {
            return new BigDecimal(content);
        }
    }

    public Node toConstNumber() {
        return new ConstNode(getNumber(content)).setPositionBegin(position);
    }
}
