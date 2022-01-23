package com.github.leeonky.interpreter;

public class NumberOverflowException extends RuntimeException {
    public NumberOverflowException(String content) {
        super(String.format("Cannon save [%s] with the given postfix type", content));
    }
}
