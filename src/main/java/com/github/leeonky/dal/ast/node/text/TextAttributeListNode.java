package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;

public class TextAttributeListNode {
    final List<DALNode> attributes;

    public TextAttributeListNode(List<DALNode> attributes) {
        this.attributes = attributes;
    }

    public TextAttribute getAttribute(RuntimeContextBuilder.DALRuntimeContext context) {
        TextAttribute[] textAttributes = attributes.stream().map(TextAttributeNode.class::cast)
                .map(node -> node.getTextAttribute(context)).toArray(TextAttribute[]::new);
        return textAttributes.length == 0 ? new TextAttribute() : textAttributes[textAttributes.length - 1];
    }

    public static class TextAttribute {
        public CharSequence newLine() {
            return "\n";
        }
    }
}
