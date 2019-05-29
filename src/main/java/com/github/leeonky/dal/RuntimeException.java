package com.github.leeonky.dal;

public class RuntimeException extends DalException {

    public RuntimeException(String message, int position) {
        super(message, position);
    }

}
