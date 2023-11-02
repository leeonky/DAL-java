package com.github.leeonky.dal.ast.node;

import java.util.List;

public class ExclamationNode extends DALNode {

    private final String exclamations;

    public ExclamationNode(List<String> exclamations) {
        this.exclamations = String.join("", exclamations);
    }

    @Override
    public String inspect() {
        return exclamations;
    }
}
