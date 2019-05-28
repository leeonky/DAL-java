package com.github.leeonky.dal;

public class RuntimeException extends java.lang.RuntimeException {
    private final int position;

    public RuntimeException(String message, int position) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }
}
