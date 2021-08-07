package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

class SchemaAssertionExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void unexpected_data_type_should_return_false() {
        assertFalse((Boolean) new SchemaAssertionExpression(new ConstNode(1), new SchemaNode("String"),
                new ConstNode(true)).evaluate(runtimeContextBuilder.build(null)));
    }

    @Test
    void should_wrapper_object_as_target_type() {
        new SchemaAssertionExpression(InputNode.INSTANCE, new SchemaNode("URL"),
                new Expression(new PropertyNode(InputNode.INSTANCE, "protocol"),
                        new Operator.Equal(),
                        new ConstNode("http")
                )).evaluate(runtimeContextBuilder.build("http://www.baidu.com"));

    }
}