package com.github.leeonky.dal;

public class SyntaxException extends RuntimeException {
    private final int position;

    public SyntaxException(int position, String message) {
        super(message, position);
        this.position = position;
    }

    @Override
    public int getPosition() {
        return position;
    }
}
