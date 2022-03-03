package com.github.leeonky.interpreter;

public class TestExpression extends TestNode implements Expression<TestContext, TestNode, TestExpression, TestOperator> {

    private final TestNode left;
    private final TestNode right;
    private final TestOperator operator;

    public TestExpression(TestNode left, TestOperator operator, TestNode right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    @Override
    public TestNode getLeftOperand() {
        return left;
    }

    @Override
    public TestNode getRightOperand() {
        return right;
    }

    @Override
    public TestOperator getOperator() {
        return operator;
    }
}
