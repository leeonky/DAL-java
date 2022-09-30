package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;

import java.util.List;

public class TextNotationNode extends DALNode {
    private final String notation;

    public TextNotationNode(List<String> ls) {
        notation = String.join("", ls);
    }

    @Override
    public String inspect() {
        return notation;
    }
}
