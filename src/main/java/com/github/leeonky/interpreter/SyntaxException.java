package com.github.leeonky.interpreter;

public class SyntaxException extends InterpreterException {

    public SyntaxException(String message, int position) {
        this(message, position, Position.Type.CHAR);
    }

    public SyntaxException(String message, int position, Position.Type type) {
        super(message.trim(), position, type);
    }
}
