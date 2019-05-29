package com.github.leeonky.dal;

public class SyntaxException extends DalException {

    public SyntaxException(int position, String message) {
        super(message, position);
    }
}
