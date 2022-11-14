package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.BuildInTextFormatter;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextFormatter;

import java.util.List;
import java.util.stream.Collectors;

public class TextBlockAttributeListNode extends DALNode {
    final List<DALNode> attributes;

    public TextBlockAttributeListNode(List<DALNode> attributes) {
        this.attributes = attributes;
    }

    public TextFormatter getFormatter(RuntimeContextBuilder.DALRuntimeContext context) {
        return attributes.stream().map(TextBlockAttributeNode.class::cast).map(node -> node.extractTextFormatter(context))
                .reduce(BuildInTextFormatter.DEFAULT, TextFormatter::merge);
    }

    @Override
    public String inspect() {
        return attributes.stream().map(DALNode::inspect).collect(Collectors.joining(" "));
    }
}
