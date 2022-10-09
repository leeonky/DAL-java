package com.github.leeonky.dal.cucumber;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;

public class StringSteps {
    private StringNotation stringNotation;

    @Given("the string content:")
    public void the_string_content(String content) {
        stringNotation = new StringNotation(content);
    }

    @When("mark an char position on {int}")
    public void markAnCharPositionOn(int position) {
        stringNotation.position(position);
    }

    @Then("got marked string content:")
    public void gotMarkedStringContent(String mark) {
        assertThat("\n" + stringNotation.result()).isEqualTo("\n" + mark);
    }

    public static class StringNotation {
        private final String content;
        private final List<Integer> positions = new ArrayList<>();

        public StringNotation(String content) {
            this.content = content;
        }

        public StringNotation position(int position) {
            if (position >= 0 && position < content.length())
                positions.add(position);
            return this;
        }

        public String result() {
            int startPosition = -1;
            String newLine = "\n";
            String content = this.content;
            StringBuilder result = new StringBuilder();
            while (true) {
                String[] lines = content.split("\r\n|\n\r|\n|\r", 2);
                result.append(lines[0]).append(markLine(startPosition, startPosition + lines[0].length(), newLine));
                if (lines.length > 1) {
                    newLine = content.substring(lines[0].length(), content.length() - lines[1].length());
                    startPosition += lines[0].length() + newLine.length();
                    content = lines[1];
                    result.append(newLine);
                } else
                    return result.toString();
            }
        }

        private String markLine(int startPosition, int endPosition, String newLine) {
            List<Integer> linePositions = positions.stream().filter(i -> i <= endPosition && i > startPosition).collect(toList());
            if (linePositions.isEmpty())
                return "";
            StringBuilder builder = new StringBuilder().append(newLine)
                    .append(String.format("%" + (linePositions.get(0) - startPosition) + "c", '^'));
            for (int i = 1; i < linePositions.size(); i++)
                builder.append(String.format("%" + (linePositions.get(i) - linePositions.get(i - 1)) + "c", '^'));
            return builder.toString();
        }
    }
}
