package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

public class WildcardNode extends DALNode {
    private final String code;

    public WildcardNode(String code) {
        this.code = code;
    }

    @Override
    public String inspect() {
        return code;
    }

    @Override
    public Data verify(DALOperator operator, DALNode actual, DALRuntimeContext context) {
        return context.wrap(true);
    }
}
