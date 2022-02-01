package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.compiler.IntegrationTestContext;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class IntegrationSteps {
    private IntegrationTestContext integrationTestContext = new IntegrationTestContext();

    @Before
    public void reset() {
        integrationTestContext = new IntegrationTestContext();
    }

    @Given("the following json:")
    public void the_following_json_data(String json) {
        integrationTestContext.givenJsonData(json);
    }

    @Given("the following java class:")
    public void the_following_java_class_data(String classCode) {
        integrationTestContext.registerJavaClass(classCode);
    }

    @Given("defined US dollar money object with the following regex")
    public void definedUSDollarMoneyObjectWithTheFollowingRegex(String regex) {
        integrationTestContext.registerUSMoney(regex);
    }

    @When("evaluate by:")
    public void evaluate_by_the_following_expression(String expression) {
        integrationTestContext.evaluate(expression);
    }

    @When("evaluate all by:")
    public void evaluate_all_by(String expression) {
        integrationTestContext.evaluateAll(expression);
    }

    @Then("the result should:")
    public void the_result_is(String verification) {
        integrationTestContext.verifyLastEvaluated(verification);
    }

    @Then("the following verification for the instance of java class {string} should pass:")
    public void the_following_assertion_for_java_class_should_pass(String className, String expression) {
        integrationTestContext.givenJavaDataByClassName(className);
        integrationTestContext.evaluate(expression);
        integrationTestContext.shouldPass();
    }

    @Then("the following verification should pass:")
    public void the_following_verification_should_pass(String expression) {
        integrationTestContext.evaluate(expression);
        integrationTestContext.shouldPass();
    }

    @Then("failed with the message:")
    public void failed_with_the_message(String message) {
        integrationTestContext.shouldFailedWith(message);
    }

    @Then("got the following notation:")
    public void got_the_following_notation(String notation) {
        integrationTestContext.shouldHaveNotation(notation);
    }
}
