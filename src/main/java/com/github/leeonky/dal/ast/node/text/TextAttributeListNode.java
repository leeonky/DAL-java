package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextAttribute;

import java.util.List;
import java.util.stream.Collectors;

public class TextAttributeListNode extends DALNode {
    final List<DALNode> attributes;

    public TextAttributeListNode(List<DALNode> attributes) {
        this.attributes = attributes;
    }

    public TextAttribute getAttribute(RuntimeContextBuilder.DALRuntimeContext context) {
        return attributes.stream().map(TextAttributeNode.class::cast).map(node -> node.getTextAttribute(context))
                .reduce(new DefaultTextAttribute(), TextAttribute::merge);
    }

    @Override
    public String inspect() {
        return attributes.stream().map(DALNode::inspect).collect(Collectors.joining(" "));
    }

    private static class DefaultTextAttribute extends TextAttribute {
        @Override
        public String newLine() {
            return "\n";
        }

        @Override
        public String tail() {
            return "<";
        }

        @Override
        public String description() {
            return "default";
        }
    }
}
