package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.FunctionUtil;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class GroupNode extends DALNode {

    private final List<DALNode> elements;
    private final List<Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode>> clauses = new ArrayList<>();

    public GroupNode(List<DALNode> elements) {
        this.elements = elements;
    }

    @Override
    public String inspect() {
        return elements.stream().map(DALNode::inspect).collect(Collectors.joining(", ", "<<", ">>"))
                + getElement(InputNode.INSTANCE).inspect();
    }

    @Override
    public boolean verifyBy(DALNode expected, DALOperator.Equal operator,
                            RuntimeContextBuilder.DALRuntimeContext context) {
        return elements.stream().allMatch(element -> {
            try {
                return getElement(element).verifyBy(expected, operator, context);
            } catch (InterpreterException e) {
                e.multiPosition(element.getOperandPosition(), InterpreterException.Position.Type.CHAR);
                throw e;
            }
        });
    }

    private DALNode getElement(DALNode element) {
        return clauses.stream().reduce(element, (e, clause) -> clause.expression(e), FunctionUtil.notAllowParallelReduce());
    }

    @Override
    public boolean verifyBy(DALNode expected, DALOperator.Matcher operator,
                            RuntimeContextBuilder.DALRuntimeContext context) {
        return elements.stream().allMatch(element -> {
            try {
                return getElement(element).verifyBy(expected, operator, context);
            } catch (InterpreterException e) {
                e.multiPosition(element.getOperandPosition(), InterpreterException.Position.Type.CHAR);
                throw e;
            }
        });
    }

    public GroupNode appendChain(Clause<RuntimeContextBuilder.DALRuntimeContext, DALNode> clause) {
        clauses.add(clause);
        return this;
    }
}
