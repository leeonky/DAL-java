package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.DalException;

public class RuntimeException extends DalException {

    public RuntimeException(String message, int position) {
        super(message, position);
    }
}
