package com.github.leeonky.dal.cucumber;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.leeonky.dal.compiler.IntegrationTestContext;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.*;
import lombok.SneakyThrows;

import java.util.List;
import java.util.Map;

public class IntegrationSteps {
    private IntegrationTestContext integrationTestContext;

    @Before
    public void reset() {
        integrationTestContext = new IntegrationTestContext();
    }

    @After
    public void release() {
        integrationTestContext.release();
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

    @Given("set the first element index to {int} of list {string}")
    public void set_the_first_element_index_to_of_list(int index, String type) {
        integrationTestContext.setArrayFirstIndex(type, index);
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

    @Then("the following verification should syntax error:")
    public void the_following_verification_should_syntax_error(String expression) {
        integrationTestContext.evaluate(expression);
        integrationTestContext.shouldSyntaxError();
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

    @Then("the inspect should:")
    public void the_inspect_should(String inspect) {
        integrationTestContext.verifyInspect(inspect);
    }

    @Then("the following expectation should pass:")
    public void theFollowingExpectationShouldPass(String dalExpression) {
        integrationTestContext.shouldPass(dalExpression);
    }

    @When("expect by the following code:")
    public void expectByTheFollowingCode(String dalExpression) {
        integrationTestContext.should(dalExpression);
    }

    @Then("assert error with the message:")
    public void assertErrorWithTheMessage(String message) {
        integrationTestContext.shouldAssertError(message);
    }

    @Then("the following exact expectation should pass:")
    public void theFollowingEqualExpectationShouldPass(String exactExpression) {
        integrationTestContext.exactPass(exactExpression);
    }

    @When("expect exact by the following code:")
    public void expectEqualByTheFollowingCode(String exactExpression) {
        integrationTestContext.exact(exactExpression);
    }

    @Then("the following matching expectation should pass:")
    public void theFollowingMatchingExpectationShouldPass(String matchingExpression) {
        integrationTestContext.matchPass(matchingExpression);
    }

    @When("expect matching by the following code:")
    public void expectMatchingByTheFollowingCode(String matchingExpression) {
        integrationTestContext.match(matchingExpression);
    }

    @Then("dumped data should be:")
    public void dumpedDataShouldBe(String verification) {
        integrationTestContext.verifyDumpedData(verification);
    }

    @Then("dumped instance of java class {string} should be:")
    public void dumpedInstanceOfJavaClassShouldBe(String type, String verification) {
        integrationTestContext.givenJavaDataByClassName(type);
        integrationTestContext.verifyDumpedData(verification);
    }

    @And("args range of java class {string} static method {string}::{string}:")
    public void argsRangeOfJavaClassStaticMethod(String type, String methodType, String method, List<List<String>> range) {
        integrationTestContext.setCurryingStaticMethodArgRange(type, methodType, method, range.get(0));
    }

    @SneakyThrows
    @And("args range of java class {string} method {string}:")
    public void argsRangeOfJavaClassMethod(String type, String method, String ranges) {
        List<Map<String, List<?>>> rangeList = new ObjectMapper().readValue(ranges, List.class);
        integrationTestContext.setCurryingMethodArgRange2(type, method, rangeList);
    }

    @Given("the following dal input:")
    public void theFollowingDalInput(String expression) {
        integrationTestContext.givenDalData(expression);
    }

    @Given("register Empty value dumper;")
    public void registerEmptyValueDumper() {
        integrationTestContext.registerEmptyValueDumper();
    }

    @Given("the following dal inputs:")
    public void theFollowingDalInputs(String expression) {
        integrationTestContext.givenDalDataList(expression);
    }

    @Then("dumped data under {int} lines should be:")
    public void dumpedDataUnderLinesShouldBe(int maxCount, String verification) {
        integrationTestContext.verifyDumpedData(verification, maxCount);
    }

    @And("register the following PropertyAccessor for java class {string}:")
    public void registerTheFollowingPropertyAccessorForJavaClass(String type, String code) {
        integrationTestContext.givenPropertyAccessor(type, code);
    }

    @Then("got the following exception:")
    public void gotTheFollowingException(String expression) {
        integrationTestContext.shouldAssertException(expression);
    }

    @When("use a instance of java class {string} to assert:")
    public void useAInstanceOfJavaClassToAssert(String type, String expression) {
        integrationTestContext.givenJavaDataByClassName(type);
        integrationTestContext.should(expression);
    }

    @Given("the following text formatter {string}:")
    public void theFollowingTextFormatter(String name, String code) {
        integrationTestContext.givenTextFormatter(name, code);
    }

    @And("register DAL:")
    public void registerDAL(String code) {
        integrationTestContext.registerDAL(code);
    }

    @Given("the following input code:")
    public void theFollowingInputCode(String code) {
        integrationTestContext.givenInputCode(code);
    }

    @Then("the following verification of input code should pass:")
    public void theFollowingVerificationOfInputCodeShouldPass(String expression) {
        integrationTestContext.shouldPassByInputCode(expression);
    }

    @When("expect run by the following code:")
    public void expectRunByTheFollowingCode(String code) {
        integrationTestContext.runShould(code);
    }

    @And("register the following BeanDALCollectionFactory for java class {string}:")
    public void registerTheFollowingBeanDataListFactoryForJavaClass(String type, String code) {
        integrationTestContext.givenDALCollectionFactory(type, code);
    }

    @But("got the following warning:")
    public void gotTheFollowingWarning(String verification) {
        integrationTestContext.shouldHaveWarning(verification);
    }
}
