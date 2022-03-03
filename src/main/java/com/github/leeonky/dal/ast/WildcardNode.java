package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public boolean verify(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public String inspect() {
        return code;
    }
}
