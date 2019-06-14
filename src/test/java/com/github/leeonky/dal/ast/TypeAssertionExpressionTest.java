package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.CompilingContextBuilder;
import com.github.leeonky.dal.RuntimeException;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TypeAssertionExpressionTest {
    CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder();

    @Test
    void unexpected_data_type_should_return_false() {
        assertFalse((Boolean) new TypeAssertionExpression(new ConstNode(1), new TypeNode("String"),
                new ConstNode(true)).evaluate(compilingContextBuilder.build(null)));
    }

    @Test
    void should_raise_runtime_error_when_wrapper_object_failed() {
        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            compilingContextBuilder.registerStringValueFormat(Wrapper.class);
            TypeNode typeNode = new TypeNode("Wrapper");
            typeNode.setPositionBegin(10);
            new TypeAssertionExpression(new ConstNode("hello"), typeNode, new ConstNode(true)).evaluate(compilingContextBuilder.build(null));
        });

        assertThat(exception)
                .hasFieldOrPropertyWithValue("position", 10)
                .hasMessage("Failed to wrap [hello] to Wrapper. Type Wrapper should have a constructor Wrapper(String)");
    }

    @Test
    void should_wrapper_object_as_target_type() {
        new TypeAssertionExpression(InputNode.INSTANCE, new TypeNode("URL"),
                new Expression(new PropertyNode(InputNode.INSTANCE, asList("protocol")),
                        new Operator.Equal(),
                        new ConstNode("http")
                )).evaluate(compilingContextBuilder.build("http://www.baidu.com"));

    }

    static class Wrapper {
        public Wrapper(String str) throws Exception {
            throw new Exception();
        }
    }
}