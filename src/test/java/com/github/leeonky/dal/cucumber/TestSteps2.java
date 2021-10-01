package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.compiler.MandatoryNodeParser;
import com.github.leeonky.dal.compiler.NodeParser;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import static com.github.leeonky.dal.compiler.ExpressionParser.*;
import static com.github.leeonky.dal.compiler.MandatoryNodeParser.*;
import static com.github.leeonky.dal.compiler.NodeParser.*;

public class TestSteps2 {
    private final Map<String, NodeParser> parserMap = new HashMap<String, NodeParser>() {{
        put("number", NUMBER);
        put("integer", INTEGER);
        put("single-quoted-string", SINGLE_QUOTED_STRING);
        put("double-quoted-string", DOUBLE_QUOTED_STRING);
        put("const-true", CONST_TRUE);
        put("const-false", CONST_FALSE);
        put("const-null", CONST_NULL);
        put("const", CONST);
        put("regex", REGEX);
        put("dot-property", DOT_PROPERTY.defaultInputNode());
        put("identity-property", IDENTITY_PROPERTY);
        put("bracket-property", BRACKET_PROPERTY.defaultInputNode());
        put("explicit-property", EXPLICIT_PROPERTY.defaultInputNode());
        put("property", PROPERTY);
        put("operand", optional(OPERAND));
        put("binary-operator-expression", BINARY_OPERATOR_EXPRESSION.defaultInputNode());
        put("schema-expression", SCHEMA_EXPRESSION.defaultInputNode());
        put("expression", optional(EXPRESSION));
        put("parentheses", PARENTHESES);
        put("object", OBJECT);
        put("list", LIST);
        put("judgement-expression-operand", optional(JUDGEMENT_EXPRESSION_OPERAND));
        put("schema", optional(SCHEMA));
    }};

    private NodeParser optional(MandatoryNodeParser mandatoryNodeParser) {
        return sourceCode -> Optional.ofNullable(mandatoryNodeParser.fetch(sourceCode));
    }

    @Before
    public void clearEnv() {
        TestContext2.reset();
    }

    @Given("the following dal code xx:")
    public void the_following_dal_code_xx(String code) {
        TestContext2.INSTANCE.giveDalSourceCode(code);
    }

    @Given("ignore an {string} node xx")
    public void ignore_an_node_xx(String factory) {
        TestContext2.INSTANCE.ignoreNodeBy(parserMap.get(factory));
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
