package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }

    @Override
    public boolean judge(DALNode actualNode, Operator.Equal operator, RuntimeContextBuilder.RuntimeContext context) {
        return true;
    }

    @Override
    public boolean judge(DALNode actualNode, Operator.Matcher operator, RuntimeContextBuilder.RuntimeContext context) {
        return true;
    }

    @Override
    public String inspect() {
        return code;
    }
}
