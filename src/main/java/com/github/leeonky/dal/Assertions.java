package com.github.leeonky.dal;

import com.github.leeonky.interpreter.InterpreterException;

public class Assertions {
    private final Object input;
    private static DAL dal = DAL.getInstance();
    public static boolean dumpInput = true;

    public static void setDal(DAL dal) {
        Assertions.dal = dal;
    }

    public static void dumpInput(boolean enable) {
        dumpInput = enable;
    }

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
            String detailMessage = "\n" + e.show(fullCode, prefix.length()) + "\n" + e.getMessage();
            if (dumpInput)
                detailMessage += "\n\nThe root value was:\n" + dal.getRuntimeContextBuilder().build(null).wrap(input).dump();
            throw new AssertionError(detailMessage);
        }
    }

    public void exact(String verification) {
        should("=", verification);
    }

    public void match(String verification) {
        should(":", verification);
    }
}
