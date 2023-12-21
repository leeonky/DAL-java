package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.ast.opt.Equal;
import com.github.leeonky.dal.ast.opt.Match;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.checker.Checker;

import java.util.List;
import java.util.stream.Stream;

public class DelegateNode extends DALNode {
    private final DALNode node;
    private Data data;

    public DelegateNode(DALNode node) {
        this.node = node;
    }

    @Override
    public Data evaluateData(RuntimeContextBuilder.DALRuntimeContext context) {
        if (data == null)
            data = node.evaluateData(context);
        return data;
    }

    @Override
    public String inspect() {
        return node.inspect();
    }

    @Override
    public int getPositionBegin() {
        return node.getPositionBegin();
    }

    @Override
    public Data verify(DALNode actualNode, Match operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return node.verify(actualNode, operator, context);
    }

    @Override
    public Data verify(DALNode actualNode, Equal operator, RuntimeContextBuilder.DALRuntimeContext context) {
        return node.verify(actualNode, operator, context);
    }

    @Override
    protected Data checkerVerify(Checker checker, Data expected, Data actual, RuntimeContextBuilder.DALRuntimeContext context) {
        return node.checkerVerify(checker, expected, actual, context);
    }

    @Override
    public List<Object> propertyChain() {
        return node.propertyChain();
    }

    @Override
    public Object getRootSymbolName() {
        return node.getRootSymbolName();
    }

    @Override
    public Stream<Object> collectFields(Data data) {
        return node.collectFields(data);
    }

    @Override
    public DALNode setPositionBegin(int positionBegin) {
        return node.setPositionBegin(positionBegin);
    }

    @Override
    public int getIndent() {
        return node.getIndent();
    }

    @Override
    public DALNode setIndent(int indent) {
        return node.setIndent(indent);
    }

    @Override
    public int getOperandPosition() {
        return node.getOperandPosition();
    }
}
