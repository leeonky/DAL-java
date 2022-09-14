package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.compiler.CucumberContextBak;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.And;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class TestSteps {

    private CucumberContextBak instance;

    @Before
    public void clearEnv() throws InterruptedException {
        instance = new CucumberContextBak();
    }

    @After
    public void release() throws InterruptedException {
        instance.release();
    }

    @Given("the following dal code:")
    public void the_following_dal_code(String code) {
        instance.giveDalSourceCode(code);
    }

    @Then("node evaluate result is:")
    public void evaluate_result_is(String assertion) {
        instance.assertLastNodeValue(assertion);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String parserPosition) {
        instance.shouldShowSourceCodePosition(parserPosition);
    }

    @When("assert by the following code:")
    public void assert_by_the_following_code(String assertionCode) {
        instance.assertInputData(assertionCode);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        instance.shouldHasDalMessage(message);
    }

    @Then("the following assertion should pass:")
    public void the_following_assertion_should_pass(String assertionCode) {
        instance.assertInputData(assertionCode);
        instance.shouldNoException(assertionCode);
    }

    @Given("the following input data:")
    public void the_following_input_data(String json) {
        instance.givenInputByJson(json);
    }

    @Given("the following schema:")
    public void the_following_schema(String schemaCode) {
        instance.registerSchema(schemaCode);
    }

    @When("evaluate by the following code:")
    public void evaluate_list_by_the_following_code(String sourceCode) {
        instance.giveDalSourceCode(sourceCode);
    }

    @Then("single result is:")
    public void single_result_is(String assertionCode) {
        instance.assertEvaluateValue(assertionCode);
    }

    @Then("multi result is:")
    public void multi_result_is(String assertionCode) {
        instance.assertEvaluateValues(assertionCode);
    }

    @Given("the following input java class data:")
    public void the_following_input_java_class_data(String javaClassDataSourceCode) {
        instance.addInputJavaClass(javaClassDataSourceCode);
    }

    @Then("the following assertion for {string} should pass:")
    public void the_following_assertion_for_should_pass(String className, String assertion) {
        instance.assertJavaClass(className, assertion);
        instance.shouldNoException(assertion);
    }

    @When("assert {string} by the following code:")
    public void assert_by_the_following_code(String className, String assertion) {
        instance.assertJavaClass(className, assertion);
    }

    @And("set the first element index to {int} of list type {string}")
    public void setTheFirstElementIndexToOfListType(int index, String type) {
        instance.setArrayFirstIndex(type, index);
    }
}
