package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.InputException;
import com.github.leeonky.dal.type.InputCode;
import com.github.leeonky.dal.type.InputValue;
import com.github.leeonky.interpreter.InterpreterException;

import java.util.function.Supplier;

public class Assertions {
    private final InputCode<Object> inputCode;
    public static boolean dumpInput = true;
    private DAL dal;
    private static Supplier<DAL> dalFactory = DAL::getInstance;

    public static void setDalFactory(Supplier<DAL> dalFactory) {
        Assertions.dalFactory = dalFactory;
    }

    public static void dumpInput(boolean enable) {
        dumpInput = enable;
    }

    private Assertions(InputCode<Object> input) {
        inputCode = input;
        dal = dalFactory.get();
    }

    public Assertions use(DAL dal) {
        this.dal = dal;
        return this;
    }

    public static Assertions expect(Object input) {
        return new Assertions((InputValue<Object>) () -> input);
    }

    public static Assertions expectRun(InputCode<Object> supplier) {
        return new Assertions(supplier);
    }

    public Assertions should(String dalExpression) {
        return should("", dalExpression);
    }

    public Assertions should(String prefix, String verification) {
        String fullCode = prefix + verification;
        try {
            dal.evaluate(inputCode, fullCode);
        } catch (InputException e) {
            String detailMessage = "\n" + e.show(fullCode, prefix.length());
            detailMessage += "\n\nInput code got exception: " + dal.getRuntimeContextBuilder().build(null).wrap(e.getInputClause()).dumpAll();
            throw new AssertionError(detailMessage);
        } catch (InterpreterException e) {
            String detailMessage = "\n" + e.show(fullCode, prefix.length()) + "\n\n" + e.getMessage();
            if (dumpInput)
                detailMessage += "\n\nThe root value was: " + dal.getRuntimeContextBuilder().build(null).wrap(inputCode).dumpAll();
            throw new AssertionError(detailMessage);
        }
        return this;
    }

    public void exact(String verification) {
        should("=", verification);
    }

    public void match(String verification) {
        should(":", verification);
    }
}
