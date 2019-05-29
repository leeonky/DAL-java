package com.github.leeonky.dal;

public class DalException extends java.lang.RuntimeException {
    private final int position;

    protected DalException(String message, int position) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }
}
