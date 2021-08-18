package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.token.TokenFactory;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;

import static org.assertj.core.api.Assertions.assertThat;

public class TestSteps {

    TestContext testContext = new TestContext();

    @Given("the follow dal code:")
    public void the_follow_dal_code(String dalSourceCode) {
        testContext.givenDalSourceCode(parseTabAndSpace(dalSourceCode));
    }

    @Then("got the following number token:")
    public void got_the_following_number_token(String assertion) {
        testContext.parseToken(TokenFactory.createNumberTokenFactory());
        testContext.assertToken(assertion);
    }

    @Then("got the following identifier token:")
    public void got_the_following_identifier_token(String assertion) {
        testContext.parseToken(TokenFactory.createIdentifierTokenFactory());
        testContext.assertToken(assertion);
    }

    @Then("current offset char of source code is {string}")
    public void current_offset_char_of_source_code_is(String character) {
        assertThat(testContext.getSourceCode().startsWith(parseTabAndSpace(character))).isTrue();
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }
}
