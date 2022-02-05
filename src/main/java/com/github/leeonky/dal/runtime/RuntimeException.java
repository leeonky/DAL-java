package com.github.leeonky.dal.runtime;

public class RuntimeException extends DalException {

    public RuntimeException(String message, int position) {
        super(message, position);
    }
}
