package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Stream;

public class ParenthesesNode extends DALNode {
    private final DALNode node;

    public ParenthesesNode(DALNode node) {
        this.node = node;
    }

    @Override
    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        return node.evaluateData(context);
    }

    @Override
    public String inspect() {
        return "(" + node.inspect() + ")";
    }


    @Override
    public Object getRootSymbolName() {
        return node.getRootSymbolName();
    }

    @Override
    public int getOperandPosition() {
        return node.getPositionBegin();
    }

    @Override
    public List<Object> propertyChain() {
        return node.propertyChain();
    }

    @Override
    public Stream<Object> collectFields(Data data) {
        return node.collectFields(data);
    }
}
