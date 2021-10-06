package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.compiler.CucumberContext;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class TestSteps {
    @Before
    public void clearEnv() {
        CucumberContext.reset();
    }

    @Given("the following dal code:")
    public void the_following_dal_code(String code) {
        CucumberContext.INSTANCE.giveDalSourceCode(code);
    }

    @Given("ignore an {string} node")
    public void ignore_an_node(String factory) {
        CucumberContext.INSTANCE.ignoreNodeBy(factory);
    }

    @Then("got the following {string} node:")
    public void got_the_following_node(String factory, String assertion) {
        CucumberContext.INSTANCE.compileAndAssertNode(factory, assertion);
    }

    @Then("evaluate result is:")
    public void evaluate_result_is(String assertion) {
        CucumberContext.INSTANCE.assertLastNodeValue(assertion);
    }

    @Then("evaluate as {string} result is:")
    public void evaluate_as_result_is(String factory, String assertion) {
        CucumberContext.INSTANCE.assertNodeValue(assertion, factory);
    }

    @Then("failed to get {string} node with the following message:")
    public void failed_to_get_the_following_node_with_the_following_message(String factory, String message) {
        CucumberContext.INSTANCE.failedToGetNodeWithMessage(factory, message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String sourceCodePosition) {
        CucumberContext.INSTANCE.shouldShowSourceCodePosition(sourceCodePosition);
    }

    @When("assert by the following code:")
    public void assert_by_the_following_code(String assertionCode) {
        CucumberContext.INSTANCE.assertInputData(assertionCode);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        CucumberContext.INSTANCE.shouldHasDalMessage(message);
    }

    @Then("the following assertion should pass:")
    public void the_following_assertion_should_pass(String assertionCode) {
        CucumberContext.INSTANCE.assertInputData(assertionCode);
        CucumberContext.INSTANCE.shouldNoException();
    }

    @Given("the following input data:")
    public void the_following_input_data(String json) {
        CucumberContext.INSTANCE.givenInputByJson(json);
    }

    @Given("the following schema:")
    public void the_following_schema(String schemaCode) {
        CucumberContext.INSTANCE.registerSchema(schemaCode);
    }
}
