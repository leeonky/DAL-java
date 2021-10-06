package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContext;

public class WildcardNode extends Node {

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        return true;
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        return true;
    }

    @Override
    public String inspect() {
        return "*";
    }
}
