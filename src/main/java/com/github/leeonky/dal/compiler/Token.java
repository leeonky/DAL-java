package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;

import java.math.BigDecimal;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;

public class Token {
    private final StringBuilder contentBuilder;
    private final int position;

    public int getPosition() {
        return position;
    }

    public Token(int position) {
        this.position = position;
        contentBuilder = new StringBuilder();
    }

    public Number getInteger() {
        String content = getContent();
        try {
            return Integer.decode(content);
        } catch (NumberFormatException ignore) {
            try {
                return Long.decode(content);
            } catch (NumberFormatException ignore2) {
                throw new SyntaxException("expect an integer", position);
            }
        }
    }

    public Number getNumber() {
        try {
            return getInteger();
        } catch (SyntaxException ignore) {
            return new BigDecimal(getContent());
        }
    }

    public String getContent() {
        return contentBuilder.toString();
    }

    public void append(char c) {
        contentBuilder.append(c);
    }

    public Token append(String str) {
        contentBuilder.append(str);
        return this;
    }

    public Node toDotProperty(Node instanceNode) {
        if (contentBuilder.length() == 0)
            throw new SyntaxException("property is not finished", position);
        return new PropertyNode(instanceNode, getContent(), DOT);
    }

    public boolean isNumber() {
        try {
            return getNumber() != null;
        } catch (Exception ignore) {
            return false;
        }
    }

    public boolean all() {
        return true;
    }
}
