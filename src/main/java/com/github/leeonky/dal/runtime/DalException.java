package com.github.leeonky.dal.runtime;

import com.github.leeonky.interpreter.InterpreterException;

public class DalException extends InterpreterException {

    public DalException(String message, int position) {
        this(message, position, Position.Type.CHAR);
    }

    public DalException(String message, int position, Position.Type type) {
        super(message, position, type);
    }
}
