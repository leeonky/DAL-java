package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Matcher;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }

    @Override
    public Data verify(DALNode actualNode, Equal operator, DALRuntimeContext context) {
        return context.wrap(true);
    }

    @Override
    public Data verify(DALNode actualNode, Matcher operator, DALRuntimeContext context) {
        return context.wrap(null);
    }

    @Override
    public String inspect() {
        return code;
    }
}
