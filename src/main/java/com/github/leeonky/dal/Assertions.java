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

    public Assertions should(String dalExpression) {
        return should("", dalExpression);
    }

    public <T> T get(String dalExpression) {
        return get("", dalExpression);
    }

    public <T> T get(String prefix, String dalExpression) {
        String fullCode = prefix + dalExpression;
        try {
            return dal.evaluate(input, fullCode);
        } catch (InterpreterException e) {
            String detailMessage = "\n" + e.show(fullCode, prefix.length()) + "\n\n" + e.getMessage();
            if (dumpInput)
                detailMessage += "\n\nThe root value was: " + dal.getRuntimeContextBuilder().build(null).wrap(input).dumpAll();
//            TODO should raise assert error only in assert method (should)
//            TODO move get out
            throw new AssertionError(detailMessage);
        }
    }

    public Assertions should(String prefix, String verification) {
        get(prefix, verification);
        return this;
    }

    public void exact(String verification) {
        should("=", verification);
    }

    public void match(String verification) {
        should(":", verification);
    }
}
