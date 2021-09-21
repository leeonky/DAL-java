package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.ast.InputNode;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

import java.util.HashMap;
import java.util.Map;

import static com.github.leeonky.dal.cucumber.ExpressionParser.BRACKET_PROPERTY;
import static com.github.leeonky.dal.cucumber.ExpressionParser.DOT_PROPERTY;
import static com.github.leeonky.dal.cucumber.NodeParser.*;

public class TestSteps2 {
    private final Map<String, NodeParser> parserMap = new HashMap<String, NodeParser>() {{
        put("number", NUMBER);
        put("single-quoted-string", SINGLE_QUOTED_STRING);
        put("double-quoted-string", DOUBLE_QUOTED_STRING);
        put("const-true", CONST_TRUE);
        put("const-false", CONST_FALSE);
        put("const-null", CONST_NULL);
        put("const", CONST);
        put("regex", REGEX);
        put("dot-property", sourceCode -> DOT_PROPERTY.fetch(sourceCode, InputNode.INSTANCE));
        put("identity-property", IDENTITY_PROPERTY);
        put("bracket-property", sourceCode -> BRACKET_PROPERTY.fetch(sourceCode, InputNode.INSTANCE));
    }};

    @Before
    public void clearEnv() {
        TestContext2.reset();
    }

    @Given("the following dal code xx:")
    public void the_following_dal_code_xx(String code) {
        TestContext2.INSTANCE.giveDalSourceCode(code);
    }

    @Then("got the following {string} node xx:")
    public void got_the_following_node_xx(String factory, String assertion) {
        TestContext2.INSTANCE.compileAndAssertNode(parserMap.get(factory), assertion);
    }

    @Then("evaluate result is xx:")
    public void evaluate_result_is_xx(String assertion) {
        TestContext2.INSTANCE.assertLastNodeValue(assertion);
    }

    @Then("evaluate as {string} result is:")
    public void evaluate_as_result_is(String factory, String assertion) {
        TestContext2.INSTANCE.assertNodeValue(assertion, parserMap.get(factory));
    }

    @Then("failed to get {string} node with the following message xx:")
    public void failed_to_get_the_following_node_with_the_following_message_xx(String factory, String message) {
        TestContext2.INSTANCE.failedToGetNodeWithMessage(parserMap.get(factory), message);
    }

    @Then("got the following source code information xx:")
    public void got_the_following_source_code_information_xx(String sourceCodePosition) {
        TestContext2.INSTANCE.shouldShowSourceCodePosition(sourceCodePosition);
    }

    @When("assert by the following code xx:")
    public void assert_by_the_following_code_xx(String assertionCode) {
        TestContext2.INSTANCE.assertInputData(assertionCode);
    }

    @Then("failed with the following message xx:")
    public void failed_with_the_following_message_xx(String message) {
        TestContext2.INSTANCE.shouldHasDalMessage(message);
    }

    @Then("the following assertion should pass xx:")
    public void the_following_assertion_should_pass_xx(String assertionCode) {
        TestContext2.INSTANCE.assertInputData(assertionCode);
        TestContext2.INSTANCE.shouldNoException();
    }

    @Given("the following input data xx:")
    public void the_following_input_data_xx(String json) {
        TestContext2.INSTANCE.givenInputByJson(json);
    }
}
