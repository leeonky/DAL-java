package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextAttribute;

public class TextAttributeNode extends DALNode {
    final String name;

    public TextAttributeNode(String Name) {
        name = Name;
    }

    @Override
    public String inspect() {
        return null;
    }

    public TextAttribute getTextAttribute(RuntimeContextBuilder.DALRuntimeContext context) {
        return context.getAttribute(name, getPositionBegin());
    }

}
