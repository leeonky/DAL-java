package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.DataAssert;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.json.JSONArray;
import org.json.JSONObject;

public class SpecSteps {
    protected static DataAssert dataAssert = new DataAssert();

    static {
        dataAssert.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JSONObjectAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayListAccessor());
    }

    private SpecContext context;

    @Before
    public void clearEnv() {
        context = new SpecContext();
    }

    @Given("the following input data:")
    public void the_following_data_to_be_tested(String json) {
        context.givenInputByJson(json);
    }

    @Then("the follow assertion should be passed:")
    public void the_follow_assertion_should_be_passed(String dalCode) {
        context.executeDal(dalCode, dataAssert);
        context.shouldPass();
    }

    @When("assert by the follow code:")
    public void assert_by_the_follow_code(String sourceCode) {
        context.executeDal(sourceCode, dataAssert);
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        context.exceptionWithMessage(message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String docString) {
        context.sourceCodePositionMessage(docString);
    }
}
