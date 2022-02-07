package com.github.leeonky.dal.ast;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

class SchemaExpressionTest {
    RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder();

    @Test
    void should_return_instance_when_type_matches() {
        SchemaExpression schemaExpression = new SchemaExpression(new ConstNode(1),
                singletonList(new SchemaNodeBak("Integer")), 0);

        assertThat(schemaExpression.evaluate(runtimeContextBuilder.build(null)))
                .isEqualTo(BigInteger.valueOf(1));
    }
}