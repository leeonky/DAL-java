package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.List;
import java.util.stream.Collectors;

public class GroupNode extends DALNode {

    private final List<DALNode> elements;

    public GroupNode(List<DALNode> elements) {
        this.elements = elements;
    }

    @Override
    public String inspect() {
        return elements.stream().map(DALNode::inspect).collect(Collectors.joining(", ", "<<", ">>"));
    }

    @Override
    public boolean verifyBy(DALNode expected, DALOperator.Equal operator,
                            RuntimeContextBuilder.DALRuntimeContext context) {
        return elements.stream().allMatch(element -> {
            try {
                return element.verifyBy(expected, operator, context);
            } catch (InterpreterException e) {
                e.multiPosition(element.getPositionBegin(), InterpreterException.Position.Type.CHAR);
                throw e;
            }
        });
    }

    @Override
    public boolean verifyBy(DALNode expected, DALOperator.Matcher operator,
                            RuntimeContextBuilder.DALRuntimeContext context) {
        return elements.stream().allMatch(element -> {
            try {
                return element.verifyBy(expected, operator, context);
            } catch (InterpreterException e) {
                e.multiPosition(element.getPositionBegin(), InterpreterException.Position.Type.CHAR);
                throw e;
            }
        });
    }
}
