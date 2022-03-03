package com.github.leeonky.interpreter;

import lombok.Data;

@Data
public class TestNode extends NodeBase<TestContext, TestNode> {
    private final Object content;

    public TestNode(Object content) {
        this.content = content;
    }

    public TestNode() {
        this(null);
    }

    public Object getContent() {
        return content;
    }
}
