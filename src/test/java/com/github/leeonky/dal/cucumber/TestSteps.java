package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.token.TokenFactory;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class TestSteps {

    private Map<String, TokenFactory> factoryMap = new HashMap<String, TokenFactory>() {{
        put("number", TokenFactory.createNumberTokenFactory());
        put("regex", TokenFactory.createRegexTokenFactory());
        put("identifier", TokenFactory.createIdentifierTokenFactory());
        put("operator", TokenFactory.createOperatorTokenFactory());
        put("dal", TokenFactory.createDALTokenFactory());
    }};

    @Given("the follow dal code:")
    public void the_follow_dal_code(String dalSourceCode) {
        TestContext.INSTANCE.givenDalSourceCode(parseTabAndSpace(dalSourceCode));
    }

    @Then("current offset char of source code is {string}")
    public void current_offset_char_of_source_code_is(String character) {
        assertThat(TestContext.INSTANCE.getSourceCode().startsWith(parseTabAndSpace(character))).isTrue();
    }

    private String parseTabAndSpace(String code) {
        return code.replace("`TAB", "\t").replace("`SPACE", " ");
    }

    @Then("got the following {string} token:")
    public void got_the_following_token(String factory, String assertion) {
        parseTokenAs(factory);
        TestContext.INSTANCE.assertToken(assertion);
    }

    @Given("take an {string} token")
    public void parseTokenAs(String factory) {
        TestContext.INSTANCE.parseToken(factoryMap.get(factory));
    }

    @Given("the follow dal code after operator {string}:")
    public void the_follow_dal_code_after_operator(String operator, String dalSourceCode) {
        TestContext.INSTANCE.givenDalSourceCode(operator + dalSourceCode);
        parseTokenAs("operator");
    }

    @Then("failed to take {string} token with the following message:")
    public void failed_to_take_token_with_the_following_message(String factory, String message) {
        TestContext.INSTANCE.failedParseToken(factoryMap.get(factory), message);
    }
}
