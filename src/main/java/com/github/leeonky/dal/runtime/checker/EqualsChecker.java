package com.github.leeonky.dal.runtime.checker;

public class EqualsChecker implements Checker {
    @Override
    public String message(CheckingContext checkingContext) {
        return checkingContext.messageEqualTo();
    }
}
