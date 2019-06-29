package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertFalse;

class TypeAssertionExpressionTest {
    CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder();

    @Test
    void unexpected_data_type_should_return_false() {
        assertFalse((Boolean) new TypeAssertionExpression(new ConstNode(1), new TypeNode("String"),
                new ConstNode(true)).evaluate(compilingContextBuilder.build(null)));
    }

    @Test
    void should_wrapper_object_as_target_type() {
        new TypeAssertionExpression(InputNode.INSTANCE, new TypeNode("URL"),
                new Expression(new PropertyNode(InputNode.INSTANCE, asList("protocol")),
                        new Operator.Equal(),
                        new ConstNode("http")
                )).evaluate(compilingContextBuilder.build("http://www.baidu.com"));

    }
}