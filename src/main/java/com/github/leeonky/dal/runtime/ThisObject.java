package com.github.leeonky.dal.runtime;

public class ThisObject implements Flatten {
    private final Data data;

    public ThisObject(Data data) {
        this.data = data;
    }

    public Data getData() {
        return data;
    }
}
