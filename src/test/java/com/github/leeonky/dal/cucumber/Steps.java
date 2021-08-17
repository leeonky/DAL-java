package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertResult;
import com.github.leeonky.dal.DalException;
import com.github.leeonky.dal.DataAssert;
import io.cucumber.java.Before;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import lombok.SneakyThrows;
import org.json.JSONArray;
import org.json.JSONObject;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class Steps {
    protected static DataAssert dataAssert = new DataAssert();

    static {
        dataAssert.getRuntimeContextBuilder()
                .registerPropertyAccessor(JSONObject.class, new JsonPropertyAccessor())
                .registerListAccessor(JSONArray.class, new JSONArrayListAccessor());
    }

    private Object inputObject;
    private String dalSourceCode = "";
    private AssertResult assertResult;
    private DalException dalException;

    @Before
    public void clearEnv() {
        inputObject = null;
        dalSourceCode = "";
        assertResult = null;
        dalException = null;
    }

    @SneakyThrows
    @Given("the following input data:")
    public void the_following_data_to_be_tested(String json) {
        inputObject = new JSONObject(json);
    }

    @Then("the follow assertion should be passed:")
    public void the_follow_assertion_should_be_passed(String dalCode) {
        assert_by_the_follow_code(dalCode);
        assertTrue(assertResult.isPassed());
    }

    @When("assert by the follow code:")
    public void assert_by_the_follow_code(String docString) {
        try {
            assertResult = dataAssert.assertData(inputObject, dalSourceCode = docString);
        } catch (DalException dalException) {
            this.dalException = dalException;
        }
    }

    @Then("failed with the following message:")
    public void failed_with_the_following_message(String message) {
        assertThat(dalException).as("expected failed but not").isNotNull();
        assertThat(dalException).hasMessage(message);
    }

    @Then("got the following source code information:")
    public void got_the_following_source_code_information(String docString) {
        assertThat(dalException.show(dalSourceCode)).isEqualTo(docString);
    }
}