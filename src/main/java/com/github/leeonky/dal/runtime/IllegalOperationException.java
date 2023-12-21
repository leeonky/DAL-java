package com.github.leeonky.dal.runtime;

import java.util.function.Supplier;

import static com.github.leeonky.dal.runtime.IllegalOperationException.Type.OP1;
import static com.github.leeonky.dal.runtime.IllegalOperationException.Type.OP2;

public class IllegalOperationException extends java.lang.RuntimeException {
    private final Type type;

    public IllegalOperationException(String message) {
        super(message);
        type = Type.OPT;
    }

    public IllegalOperationException(Exception e, Type type) {
        super(e);
        this.type = type;
    }

    public Type type() {
        return type;
    }

    public enum Type {
        OP1, OP2, OPT
    }

    public static <T> T opt1(Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw new IllegalOperationException(e, OP1);
        }
    }

    public static <T> T opt2(Supplier<T> supplier) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw new IllegalOperationException(e, OP2);
        }
    }
}
