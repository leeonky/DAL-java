package com.github.leeonky.dal;

public class SyntexException extends RuntimeException {
    private final int position;

    public SyntexException(int position, String message) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }
}
