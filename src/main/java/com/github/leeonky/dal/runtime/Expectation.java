package com.github.leeonky.dal.runtime;

public interface Expectation {

    Data equalTo(Data actual);

    Data matches(Data actual);
}
