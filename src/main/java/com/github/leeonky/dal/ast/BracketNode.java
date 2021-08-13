package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public class BracketNode extends Node {
    private final Node node;

    public BracketNode(Node node) {
        this.node = node;
    }

    @Override
    public Object evaluate(RuntimeContext context) {
        return node.evaluate(context);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof BracketNode && node.equals(((BracketNode) obj).node);
    }

    public BracketNode finishBracket() {
        return this;
    }

    @Override
    public String inspect() {
        return String.format("(%s)", node.inspect());
    }
}
