package com.github.leeonky.dal;

import com.github.leeonky.interpreter.InterpreterException;

public class Assertions {
    private final Object input;
    private final DAL dal = DAL.getInstance();

    private Assertions(Object input) {
        this.input = input;
    }

    public static Assertions expect(Object input) {
        return new Assertions(input);
    }

    public void should(String dalExpression) {
        should("", dalExpression);
    }

    public void should(String prefix, String verification) {
        String fullCode = prefix + verification;
        try {
            dal.evaluate(input, fullCode);
        } catch (InterpreterException e) {
            throw new AssertionError("\n" + e.show(fullCode, prefix.length()) + "\n" + e.getMessage());
        }
    }

    public void exact(String verification) {
        should("=", verification);
    }

    public void match(String verification) {
        should(":", verification);
    }
}
