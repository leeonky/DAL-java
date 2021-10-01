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

public class TestSteps {
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
        TestContext.reset();
    }

    @Given("the following dal code:")
    public void the_following_dal_code(String code) {
        TestContext.INSTANCE.giveDalSourceCode(code);
    }

    @Given("ignore an {string} node")
    public void ignore_an_node(String factory) {
        TestContext.INSTANCE.ignoreNodeBy(parserMap.get(factory));
    }

    @Then("got the following {string} node:")
    public void got_the_following_node(String factory, String assertion) {
        TestContext.INSTANCE.compileAndAssertNode(parserMap.get(factory), assertion);
    }

    @Then("evaluate result is:")
    public void evaluate_result_is(String assertion) {
        TestContext.INSTANCE.assertLastNodeValue(assertion);
    }

    @Then("evaluate as {string} result is:")
    public void evaluate_as_result_is(String factory, String assertion) {
        TestContext.INSTANCE.assertNodeValue(assertion, parserMap.get(factory));
    }

    @Then("failed to get {string} node with the following message:")
    public void failed_to_get_the_following_node_with_the_following_message(String factory, String message) {
        TestContext.INSTANCE.failedToGetNodeWithMessage(parserMap.get(factory), message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String sourceCodePosition) {
        TestContext.INSTANCE.shouldShowSourceCodePosition(sourceCodePosition);
    }

    @When("assert by the following code:")
    public void assert_by_the_following_code(String assertionCode) {
        TestContext.INSTANCE.assertInputData(assertionCode);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        TestContext.INSTANCE.shouldHasDalMessage(message);
    }

    @Then("the following assertion should pass:")
    public void the_following_assertion_should_pass(String assertionCode) {
        TestContext.INSTANCE.assertInputData(assertionCode);
        TestContext.INSTANCE.shouldNoException();
    }

    @Given("the following input data:")
    public void the_following_input_data(String json) {
        TestContext.INSTANCE.givenInputByJson(json);
    }

    @Given("the following schema:")
    public void the_following_schema(String schemaCode) {
        TestContext.INSTANCE.registerSchema(schemaCode);
    }
}
