package com.github.leeonky.dal;

public class SyntaxException extends DalException {

    public SyntaxException(String message, int position) {
        super(message, position);
    }
}
