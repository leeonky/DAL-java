package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.RuntimeException;
import com.github.leeonky.dal.runtime.TextFormatter;

import java.util.List;
import java.util.stream.Collectors;

import static java.lang.String.format;

public class TextBlockAttributeListNode extends DALNode {
    final List<DALNode> attributes;

    public TextBlockAttributeListNode(List<DALNode> attributes) {
        this.attributes = attributes;
    }

    @SuppressWarnings("unchecked")
    public <T> TextFormatter<String, T> getFormatter(RuntimeContextBuilder.DALRuntimeContext context) {
        Class<?> accept = String.class;
        TextFormatter textFormatter = TextFormatter.DEFAULT;
        for (DALNode attribute : attributes) {
            TextBlockAttributeNode attributeNode = (TextBlockAttributeNode) attribute;
            TextFormatter eachFormatter = attributeNode.extractTextFormatter(context);
            if (!context.getConverter().supported(accept, eachFormatter.acceptType()))
                throw new RuntimeException(format("Invalid text formatter, expect a formatter which accept %s but %s",
                        accept.getName(), eachFormatter.acceptType().getName()), attributeNode.getPositionBegin());
            accept = eachFormatter.returnType();
            textFormatter = textFormatter.merge(eachFormatter);
        }
        return textFormatter;
    }

    @Override
    public String inspect() {
        return attributes.stream().map(DALNode::inspect).collect(Collectors.joining(" "));
    }
}
