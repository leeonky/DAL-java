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

    @Given("the following dal expression:")
    public void the_following_dal_expression(String expression) {
        integrationTestContext.givenDALExpression(expression);
    }

    @Given("the following schema class:")
    public void the_following_schema_class(String schema) {
        integrationTestContext.givenSchemaClass(schema);
    }

    @When("evaluate by:")
    public void evaluate_by_the_following_expression(String expression) {
        integrationTestContext.evaluate(expression);
    }

    @When("evaluate all by:")
    public void evaluate_all_by(String expression) {
        integrationTestContext.evaluateAll(expression);
    }

    @When("use a instance of java class {string} to evaluate:")
    public void use_a_instance_of_java_class_to_evaluate(String className, String expression) {
        integrationTestContext.givenJavaDataByClassName(className);
        integrationTestContext.evaluate(expression);
    }

    @When("evaluate follow expression as {string} node:")
    public void evaluate_follow_expression_as_node(String nodeType, String expression) {
        integrationTestContext.parseAndEvaluate(expression, nodeType);
    }

    @Then("the result should:")
    public void the_result_is(String verification) {
        integrationTestContext.verifyLastEvaluated(verification);
    }

    @Then("the following verification for the instance of java class {string} should failed:")
    public void the_following_verification_for_the_instance_of_java_class_should_failed(String className, String expression) {
        integrationTestContext.givenJavaDataByClassName(className);
        integrationTestContext.evaluate(expression);
        integrationTestContext.shouldFailed();
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

    @Then("the following verification should failed:")
    public void the_following_verification_should_failed(String expression) {
        integrationTestContext.evaluate(expression);
        integrationTestContext.shouldFailed();
    }

    @Then("failed with the message:")
    public void failed_with_the_message(String message) {
        integrationTestContext.shouldFailedWith(message);
    }

    @Then("got the following notation:")
    public void got_the_following_notation(String notation) {
        integrationTestContext.shouldHaveNotation(notation);
    }

    @Then("parse the following {string} node:")
    public void parse_the_following_node(String nodeType, String verification) {
        integrationTestContext.verifyNode(nodeType, verification);
    }

    @Then("failed to parse {string} with the following message:")
    public void failed_to_parse_with_the_following_message(String nodeType, String message) {
        integrationTestContext.parseNode(nodeType);
        integrationTestContext.shouldFailedWith(message);
    }

    @Then("last evaluated node result is:")
    public void last_evaluated_node_result_is(String verification) {
        integrationTestContext.evaluateLast();
        integrationTestContext.verifyLastEvaluated(verification);
    }
}
