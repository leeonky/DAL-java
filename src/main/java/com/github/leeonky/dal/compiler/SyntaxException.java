package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.runtime.DalException;

public class SyntaxException extends DalException {

    public SyntaxException(String message, int position) {
        super(message.trim(), position);
    }

    public SyntaxException(String message, int position, Position.Type type) {
        super(message.trim(), position, type);
    }
}
