package com.github.leeonky.dal;

public class AssertionFailure extends DalException {
    public AssertionFailure(String message, int position) {
        super(message, position);
    }
}
