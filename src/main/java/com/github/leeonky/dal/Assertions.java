package com.github.leeonky.dal;

import com.github.leeonky.interpreter.InterpreterException;

import java.util.function.Supplier;

public class Assertions {
    private final Object input;
    public static boolean dumpInput = true;
    private DAL dal;
    private static Supplier<DAL> dalFactory = DAL::getInstance;

    public static void setDalFactory(Supplier<DAL> dalFactory) {
        Assertions.dalFactory = dalFactory;
    }

    public static void dumpInput(boolean enable) {
        dumpInput = enable;
    }

    private Assertions(Object input) {
        this.input = input;
        dal = dalFactory.get();
    }

    public Assertions use(DAL dal) {
        this.dal = dal;
        return this;
    }

    public static Assertions expect(Object input) {
        return new Assertions(input);
    }

    public <T> T should(String dalExpression) {
        return should("", dalExpression);
    }

    public <T> T get(String dalExpression) {
        return should(dalExpression);
    }

    public <T> T should(String prefix, String verification) {
        String fullCode = prefix + verification;
        try {
            return dal.evaluate(input, fullCode);
        } catch (InterpreterException e) {
            String detailMessage = "\n" + e.show(fullCode, prefix.length()) + "\n\n" + e.getMessage();
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
