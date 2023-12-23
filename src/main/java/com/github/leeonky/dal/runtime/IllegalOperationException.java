package com.github.leeonky.dal.runtime;

@Deprecated
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

}
