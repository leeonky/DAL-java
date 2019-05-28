package com.github.leeonky.dal;

public class SyntexException extends RuntimeException {
    private final int position;

    public SyntexException(int position, String message) {
        super(message, position);
        this.position = position;
    }

    @Override
    public int getPosition() {
        return position;
    }
}
