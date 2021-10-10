package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class ParenthesesNode extends Node {
    private final Node node;

    public ParenthesesNode(Node node) {
        this.node = node;
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.RuntimeContext context) {
        return node.evaluate(context);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ParenthesesNode && node.equals(((ParenthesesNode) obj).node);
    }

    @Override
    public String inspect() {
        return String.format("(%s)", node.inspect());
    }
}
