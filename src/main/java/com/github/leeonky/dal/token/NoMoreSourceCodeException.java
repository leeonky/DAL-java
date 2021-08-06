package com.github.leeonky.dal.token;

import com.github.leeonky.dal.DalException;

public class NoMoreSourceCodeException extends DalException {

    protected NoMoreSourceCodeException(int position) {
        super("at the end of source code", position);
    }
}
