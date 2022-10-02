package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Collectors;

public class TextBlockNode extends DALNode {
    private final NotationAttributeNode notationAttributeNode;
    private final List<Character> content;

    public TextBlockNode(NotationAttributeNode notationAttributeNode, List<Character> content) {
        this.notationAttributeNode = notationAttributeNode;
        this.content = content;
    }

    @Override
    public Object evaluate(RuntimeContextBuilder.DALRuntimeContext context) {
        return notationAttributeNode.text(content, context);
    }

    @Override
    public String inspect() {
        return notationAttributeNode.inspect() + "\n"
               + content.stream().map(Object::toString).collect(Collectors.joining())
               + notationAttributeNode.endNotation();
    }
}
