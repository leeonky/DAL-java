package com.github.leeonky.dal.ast;

@Deprecated
public class ListTailNode extends Node {
    @Override
    public String inspect() {
        return "...";
    }
}
