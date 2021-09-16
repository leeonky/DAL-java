package com.github.leeonky.dal.cucumber;

import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class SpecSteps {

    @Before
    public void clearEnv() {
        TestContext.reset();
    }

    @Given("the following input data:")
    public void the_following_data_to_be_tested(String json) {
        TestContext.INSTANCE.givenInputByJson(json);
    }

    @Then("the following assertion should pass:")
    public void the_follow_assertion_should_be_pass(String dalCode) {
        TestContext.INSTANCE.executeDal(dalCode);
        TestContext.INSTANCE.shouldPass(dalCode);
    }

    @When("assert by the following code:")
    public void assert_by_the_follow_code(String sourceCode) {
        TestContext.INSTANCE.executeDal(sourceCode);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        TestContext.INSTANCE.exceptionWithMessage(message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String docString) {
        TestContext.INSTANCE.sourceCodePositionMessage(docString);
    }
}
