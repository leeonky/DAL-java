package com.github.leeonky.interpreter;

public class TestOperator extends Operator<TestContext, TestNode, TestOperator> {

    public TestOperator() {
        super(0, "");
    }

    @Override
    public Object calculate(TestNode node1, TestNode node2, TestContext context) {
        return null;
    }
}
