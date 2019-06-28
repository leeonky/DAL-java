package com.github.leeonky.dal.format;

import com.github.leeonky.dal.token.IllegalTypeException;

import java.math.BigInteger;

public class PositiveInteger implements Formatter<Number> {

    @Override
    public Object toValue(Object input) {
        String val = input.toString();
        if (val.chars().anyMatch(c -> !Character.isDigit(c)))
            throw new IllegalTypeException();
        BigInteger value = new BigInteger(val);
        if (value.compareTo(BigInteger.ZERO) <= 0)
            throw new IllegalTypeException();
        return value;
    }
}
