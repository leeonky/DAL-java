package com.github.leeonky.dal.runtime;

import com.github.leeonky.interpreter.InterpreterException;

public class DALException extends InterpreterException {

    public DALException(String message, int position) {
        this(message, position, Position.Type.CHAR);
    }

    public DALException(String message, int position, Position.Type type) {
        super(message, position, type);
    }
}
