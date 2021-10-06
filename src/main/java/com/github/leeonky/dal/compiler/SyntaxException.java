package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.runtime.DalException;

public class SyntaxException extends DalException {

    public SyntaxException(String message, int position) {
        super(message, position);
    }
}
