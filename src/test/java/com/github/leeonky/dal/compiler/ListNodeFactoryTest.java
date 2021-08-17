package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.ListNode;
import com.github.leeonky.dal.ast.Node;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.operatorToken;
import static org.assertj.core.api.Assertions.assertThat;

class ListNodeFactoryTest extends NodeFactoryTestBase {

    @Override
    protected NodeFactory getDefaultNodeFactory() {
        return NodeFactory.createListNodeFactory();
    }

    @Test
    void return_null_when_dost_not_match() {
        assertThat(fetchNodeWhenGivenToken(operatorToken("+")))
                .isNull();
    }

    @Test
    void support_object_with_no_field() {
        Node node = givenCode("[]").fetchNode();

        assertThat(node).isInstanceOf(ListNode.class);
        assertThat(node.inspect()).isEqualTo("[]");
    }

//    @Test
//    void raise_error_when_no_closing_brace() {
//        assertThat(invalidSyntaxToken(givenCode("[")))
//                .hasMessage("missed `]`")
//                .hasFieldOrPropertyWithValue("position", 1);
//    }
//
//    @Test
//    void support_list_with_element() {
//        Node node = givenCode("[1]").fetchNode();
//
//        assertThat(node).isInstanceOf(ListNode.class);
//        assertThat(node.inspect()).isEqualTo("[1]");
//    }

    //TODO element match/equal
    //TODO nested list
    //TODO nested object
}
