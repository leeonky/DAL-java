package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextFormatter;

public class TextBlockAttributeNode extends DALNode {
    final String name;

    public TextBlockAttributeNode(String Name) {
        name = Name;
    }

    @Override
    public String inspect() {
        return name;
    }

    public TextFormatter extractTextFormatter(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.fetchFormatter(name, getPositionBegin());
    }
}
