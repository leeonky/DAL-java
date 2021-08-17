package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContext;
import com.github.leeonky.dal.util.DataObject;

public class ListNode extends Node {
    @Override
    public String inspect() {
        return "[]";
    }

    @Override
    public boolean judge(Node actualNode, Operator.Equal operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        DataObject data = context.wrap(actual);
        return data.getListSize() == 0;
    }

    @Override
    public boolean judge(Node actualNode, Operator.Matcher operator, RuntimeContext context) {
        Object actual = actualNode.evaluate(context);
        DataObject data = context.wrap(actual);
        return data.getListSize() == 0;
    }
}
