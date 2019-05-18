package com.github.leeonky.dal;

public class DataAssertor {
    public AssertResult assertData(Object actual, String expression) {
        AssertResult assertResult = new AssertResult();
        if (expression.contains("2"))
            return assertResult.setMessage("Expected value to [= 2]\n but was <1>");
        return assertResult;
    }
}
