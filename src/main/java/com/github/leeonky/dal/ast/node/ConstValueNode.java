package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.node.table.RowHeader;
import com.github.leeonky.dal.ast.node.table.RowType;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

public class ConstValueNode extends DALNode {

    private final Object value;

    public ConstValueNode(Object value) {
        this.value = value;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        return value;
    }

    @Override
    public String inspect() {
        if (value == null)
            return "null";
        if (value instanceof String)
            return String.format("'%s'", value);
        return value.toString();
    }

    @Override
    public RowType guessTableHeaderType() {
        return RowHeader.SPECIFY_INDEX;
    }

    @Override
    public boolean needPostBlankWarningCheck() {
        return true;
    }
}
