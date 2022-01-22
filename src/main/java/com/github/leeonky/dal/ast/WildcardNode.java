package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return true;
    }

    @Override
    public String inspect() {
        return code;
    }
}
