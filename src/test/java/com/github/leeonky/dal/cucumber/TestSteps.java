package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.compiler.CucumberContextBak;
import io.cucumber.java.Before;
import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class TestSteps {
    @Before
    public void clearEnv() {
        CucumberContextBak.reset();
    }

    @Given("the following dal code:")
    public void the_following_dal_code(String code) {
        CucumberContextBak.INSTANCE.giveDalSourceCode(code);
    }

    @Given("ignore an {string} node")
    public void ignore_an_node(String factory) {
        CucumberContextBak.INSTANCE.ignoreNodeBy(factory);
    }

    @Then("got the following {string} node:")
    public void got_the_following_node(String factory, String assertion) {
        CucumberContextBak.INSTANCE.compileAndAssertNode(factory, assertion);
    }

    @Then("node evaluate result is:")
    public void evaluate_result_is(String assertion) {
        CucumberContextBak.INSTANCE.assertLastNodeValue(assertion);
    }

    @Then("node evaluate as {string} result is:")
    public void evaluate_as_result_is(String factory, String assertion) {
        CucumberContextBak.INSTANCE.assertNodeValue(assertion, factory);
    }

    @Then("failed to get {string} node with the following message:")
    public void failed_to_get_the_following_node_with_the_following_message(String factory, String message) {
        CucumberContextBak.INSTANCE.failedToGetNodeWithMessage(factory, message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String parserPosition) {
        CucumberContextBak.INSTANCE.shouldShowSourceCodePosition(parserPosition);
    }

    @When("assert by the following code:")
    public void assert_by_the_following_code(String assertionCode) {
        CucumberContextBak.INSTANCE.assertInputData(assertionCode);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        CucumberContextBak.INSTANCE.shouldHasDalMessage(message);
    }

    @Then("the following assertion should pass:")
    public void the_following_assertion_should_pass(String assertionCode) {
        CucumberContextBak.INSTANCE.assertInputData(assertionCode);
        CucumberContextBak.INSTANCE.shouldNoException(assertionCode);
    }

    @Given("the following input data:")
    public void the_following_input_data(String json) {
        CucumberContextBak.INSTANCE.givenInputByJson(json);
    }

    @Given("the following schema:")
    public void the_following_schema(String schemaCode) {
        CucumberContextBak.INSTANCE.registerSchema(schemaCode);
    }

    @When("evaluate by the following code:")
    public void evaluate_list_by_the_following_code(String sourceCode) {
        CucumberContextBak.INSTANCE.giveDalSourceCode(sourceCode);
    }

    @Then("single result is:")
    public void single_result_is(String assertionCode) {
        CucumberContextBak.INSTANCE.assertEvaluateValue(assertionCode);
    }

    @Then("multi result is:")
    public void multi_result_is(String assertionCode) {
        CucumberContextBak.INSTANCE.assertEvaluateValues(assertionCode);
    }

    @Given("the following input java class data:")
    public void the_following_input_java_class_data(String javaClassDataSourceCode) {
        CucumberContextBak.INSTANCE.addInputJavaClass(javaClassDataSourceCode);
    }

    @Then("the following assertion for {string} should pass:")
    public void the_following_assertion_for_should_pass(String className, String assertion) {
        CucumberContextBak.INSTANCE.assertJavaClass(className, assertion);
        CucumberContextBak.INSTANCE.shouldNoException(assertion);
    }

    @When("assert {string} by the following code:")
    public void assert_by_the_following_code(String className, String assertion) {
        CucumberContextBak.INSTANCE.assertJavaClass(className, assertion);
    }

    @And("set the first element index to {int} of list type {string}")
    public void setTheFirstElementIndexToOfListType(int index, String type) {
        CucumberContextBak.INSTANCE.setArrayFirstIndex(type, index);
    }
}
