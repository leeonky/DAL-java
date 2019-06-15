package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigInteger;

public class PositiveInteger {
    private BigInteger value;

    public PositiveInteger(Number number) {
        String val = number.toString();
        if (val.chars().anyMatch(c -> !Character.isDigit(c)))
            throw new IllegalTypeException();
        value = new BigInteger(val);
        if (value.compareTo(BigInteger.ZERO) <= 0)
            throw new IllegalTypeException();
    }
}
