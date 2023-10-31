package com.github.leeonky.dal.runtime.checker;

public class ListScopeChecker implements Checker {
    @Override
    public String message(CheckingContext checkingContext) {
        return "no message";
    }

    @Override
    public boolean failed(CheckingContext checkingContext) {
        return false;
    }
}
