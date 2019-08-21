package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;

public class BracketNode extends Node {
    private Node node;
    private boolean finished = false;

    @Override
    public Object evaluate(RuntimeContext context) {
        return node.evaluate(context);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof BracketNode && node.equals(((BracketNode) obj).node);
    }

    public BracketNode setNode(Node node) {
        this.node = node;
        return this;
    }

    public boolean isFinished() {
        return finished;
    }

    public BracketNode finishBracket() {
        finished = true;
        return this;
    }
}
