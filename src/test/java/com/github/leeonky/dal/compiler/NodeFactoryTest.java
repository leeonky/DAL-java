package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.compiler.NodeFactory.createConstNodeFactory;
import static com.github.leeonky.dal.token.Token.constValueToken;
import static com.github.leeonky.dal.token.Token.operatorToken;
import static org.assertj.core.api.Assertions.assertThat;

class NodeFactoryTest {

    @Nested
    class FetchConstNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return createConstNodeFactory();
        }

        @Test
        void matches_and_return_const_node() {
            assertThat(fetchNodeWhenGivenToken(constValueToken("const string"), 10))
                    .isInstanceOf(ConstNode.class)
                    .hasFieldOrPropertyWithValue("value", "const string")
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
        }

        @Test
        void return_null_when_dost_not_match() {
            assertThat(fetchNodeWhenGivenToken(operatorToken("+")))
                    .isNull();
        }

        @Test
        void return_null_when_no_token() {
            assertThat(givenToken().fetchNode())
                    .isNull();
        }
    }
}
