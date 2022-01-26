package com.github.leeonky.interpreter;

public class TestExpression implements Expression<TestContext, TestNode, TestExpression, TestOperator> {
    @Override
    public TestNode getLeftOperand() {
        return null;
    }

    @Override
    public TestNode getRightOperand() {
        return null;
    }

    @Override
    public TestOperator getOperator() {
        return null;
    }

    @Override
    public int getPositionBegin() {
        return 0;
    }

    @Override
    public TestNode setPositionBegin(int positionBegin) {
        return null;
    }

    @Override
    public int getOperandPosition() {
        return 0;
    }
}
