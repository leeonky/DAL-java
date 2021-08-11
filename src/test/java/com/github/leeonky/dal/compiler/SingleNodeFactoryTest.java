package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ConstNode;
import com.github.leeonky.dal.ast.Node;
import com.github.leeonky.dal.ast.PropertyNode;
import com.github.leeonky.dal.ast.RegexNode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.compiler.NodeFactory.createConstNodeFactory;
import static com.github.leeonky.dal.compiler.NodeFactory.createPropertyNodeFactory;
import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class SingleNodeFactoryTest {

    @Nested
    class FetchConstNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return createConstNodeFactory();
        }

        @Test
        void matches_and_return_node() {
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
    }

    @Nested
    class FetchPropertyNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return createPropertyNodeFactory();
        }

        @Test
        void matches_and_return_node() {
            Node node = fetchNodeWhenGivenToken(propertyToken("name"), 10);

            assertThat(node)
                    .isInstanceOf(PropertyNode.class)
                    .hasFieldOrPropertyWithValue("name", "name")
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo(".name");
        }

        @Test
        void return_null_when_dost_not_match() {
            assertThat(fetchNodeWhenGivenToken(operatorToken("+")))
                    .isNull();
        }
    }

    @Nested
    class FetchRegexNode extends NodeFactoryTestBase {

        @Override
        protected NodeFactory getDefaultNodeFactory() {
            return NodeFactory.createRegexNodeFactory();
        }

        @Test
        void matches_and_return_node() {
            Node node = fetchNodeWhenGivenToken(regexToken("regex"), 10);

            assertThat(node)
                    .isInstanceOf(RegexNode.class)
                    .hasFieldOrPropertyWithValue("positionBegin", 10);
            assertThat(node.inspect()).isEqualTo("/regex/");
        }

        @Test
        void return_null_when_dost_not_match() {
            assertThat(fetchNodeWhenGivenToken(operatorToken("+")))
                    .isNull();
        }
    }
}
