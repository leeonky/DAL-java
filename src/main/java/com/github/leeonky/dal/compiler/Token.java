package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.ast.*;

import java.math.BigDecimal;

import static com.github.leeonky.dal.ast.PropertyNode.Type.DOT;
import static com.github.leeonky.dal.ast.PropertyNode.Type.IDENTIFIER;

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

    private Object getInteger(String content) {
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

    private static Number getNumber(String content) {
        try {
            return Integer.decode(content);
        } catch (NumberFormatException ignore) {
            try {
                return Long.decode(content);
            } catch (NumberFormatException ignore2) {
                return new BigDecimal(content);
            }
        }
    }

    private String getContent() {
        return contentBuilder.toString();
    }

    public Token append(char c) {
        contentBuilder.append(c);
        return this;
    }

    public Token append(String str) {
        contentBuilder.append(str);
        return this;
    }

    public boolean contentEmpty() {
        return contentBuilder.length() == 0;
    }

    public Node toConstNumber() {
        return new ConstNode(getNumber(getContent())).setPositionBegin(position);
    }

    public Node toDotProperty(Node instanceNode) {
        return new PropertyNode(instanceNode, getContent(), DOT).setPositionBegin(position);
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

    public Node toIdentityProperty() {
        return new PropertyNode(InputNode.INSTANCE, getContent(), IDENTIFIER).setPositionBegin(position);
    }

    public Node toConstInteger() {
        return new ConstNode(getInteger(getContent())).setPositionBegin(position);
    }

    public SchemaNode toSchemaNode() {
        return (SchemaNode) new SchemaNode(getContent()).setPositionBegin(position);
    }

    public WildcardNode toWildcardNode() {
        return (WildcardNode) new WildcardNode().setPositionBegin(position);
    }

    public boolean isNumber() {
        try {
            getNumber(getContent());
            return true;
        } catch (Exception ignore) {
            return false;
        }
    }
}
