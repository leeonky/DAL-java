package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.NodeFactory;
import com.github.leeonky.dal.token.TokenFactory;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class TestSteps {
    private final Map<String, TokenFactory> tokenFactoryMap = new HashMap<String, TokenFactory>() {{
        put("number", TokenFactory.createNumberTokenFactory());
        put("regex", TokenFactory.createRegexTokenFactory());
        put("identifier", TokenFactory.createIdentifierTokenFactory());
        put("operator", TokenFactory.createOperatorTokenFactory());
        put("dal", TokenFactory.createDALTokenFactory());
        put("(", TokenFactory.createOpeningParenthesisTokenFactory());
        put(")", TokenFactory.createClosingParenthesisTokenFactory());
        put("[", TokenFactory.createOpeningBracketTokenFactory());
        put("]", TokenFactory.createClosingBracketTokenFactory());
        put("{", TokenFactory.createOpeningBraceTokenFactory());
        put("}", TokenFactory.createClosingBraceTokenFactory());
        put("double-quoted-string", TokenFactory.createDoubleQuotedStringTokenFactory());
        put("single-quoted-string", TokenFactory.createSingleQuotedStringTokenFactory());
        put("keyWord", TokenFactory.createIdentifierTokenFactory());
        put("property", TokenFactory.createBeanPropertyTokenFactory());
        put("bracket-property", TokenFactory.createBracketPropertyTokenFactory());
    }};

    private final Map<String, NodeFactory> nodeFactoryMap = new HashMap<String, NodeFactory>() {{
        put("const", NodeFactory.CONST);
        put("parentheses", NodeFactory.PARENTHESES);
        put("regex", NodeFactory.REGEX);
        put("property", NodeFactory.PROPERTY);
        put("identifier-property", NodeFactory.IDENTIFIER_PROPERTY);
        put("explicit-property", NodeFactory.EXPLICIT_PROPERTY.inThis());
        put("bean-property", NodeFactory.BEAN_PROPERTY.inThis());
        put("bracket-property", NodeFactory.BRACKET_PROPERTY.inThis());
        put("single-evaluable", NodeFactory.OPERAND);
        put("right-operand", NodeFactory.RIGHT_OPERAND);
        put("expression", NodeFactory.EXPRESSION);
        put("object", NodeFactory.OBJECT);
        put("list", NodeFactory.LIST);
    }};

    @Given("the following dal code:")
    public void the_follow_dal_code(String dalSourceCode) {
        TestContext.INSTANCE.givenDalSourceCode(parseTabAndSpace(dalSourceCode));
    }

    @Then("current offset char of source code is {string}")
    public void current_offset_char_of_source_code_is(String character) {
        assertThat(TestContext.INSTANCE.getSourceCode().startsWith(parseTabAndSpace(character))).isTrue();
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    @Then("got the following {string} token:")
    public void got_the_following_token(String factory, String assertion) {
        parseTokenAs(factory);
        TestContext.INSTANCE.assertToken(assertion);
    }

    @Given("take an {string} token")
    public void parseTokenAs(String factory) {
        TestContext.INSTANCE.parseToken(tokenFactoryMap.get(factory));
    }

    @Given("the following dal code after operator {string}:")
    public void the_follow_dal_code_after_operator(String operator, String dalSourceCode) {
        TestContext.INSTANCE.givenDalSourceCode(operator + dalSourceCode);
        parseTokenAs("operator");
    }

    @Then("failed to take {string} token with the following message:")
    public void failed_to_take_token_with_the_following_message(String tokenFactory, String message) {
        TestContext.INSTANCE.failedParseToken(tokenFactoryMap.get(tokenFactory), message);
    }

    @Then("failed to get {string} node with the following message:")
    public void failed_to_get_node_with_the_following_message(String nodeFactory, String message) {
        TestContext.INSTANCE.failedCompileNode(nodeFactoryMap.get(nodeFactory), message);
    }

    @Then("got the following {string} node:")
    public void got_the_following_node(String nodeFactory, String assertion) {
        get_an_node(nodeFactory);
        TestContext.INSTANCE.assertNode(assertion);
    }

    @Then("evaluate result is:")
    public void evaluate_result_is(String assertion) {
        TestContext.INSTANCE.assertEvaluateNode(assertion);
    }

    @Given("get an {string} node")
    public void get_an_node(String nodeFactory) {
        TestContext.INSTANCE.compileNode(nodeFactoryMap.get(nodeFactory));
    }


    @Given("the following dal code and skip {int} tokens:")
    public void the_following_dal_code_and_skip_tokens(int skip, String dalSourceCode) {
        TestContext.INSTANCE.givenDalSourceCode(dalSourceCode);
        TestContext.INSTANCE.skipTokens(skip);
    }
}
