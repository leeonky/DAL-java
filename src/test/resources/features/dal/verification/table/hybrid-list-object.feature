Feature: hybrid object by row header

  Rule: group expression

    Scenario: resolve input as list default
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      : | name  | age |
        | Tom   | 4   |
        | Jerry | 2   |
      """

    Scenario: resolve input as list with index
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      : | name  | age |
      0 | Tom   | 4   |
      """

    Scenario: resolve input as list with [index]
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      :   | name  | age |
      [0] | Tom   | 4   |
      """

    Scenario: resolve input as list with index and [index]
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      :   | name  | age |
       0  | Tom   | 4   |
      [0] | Tom   | 4   |
      """

    Scenario: resolve input as list with index and group node with index
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
        : | name  | age |
        0 | Tom   | 4   |
    <<0>> | Tom   | 4   |
      """

    Scenario: resolve input as object with index and group node with property
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }, {
        "value": {
          "name": "John",
          "age": 19
        }
      }]
      """
      Then the following verification should pass:
      """
      :             | name | age |
                  0 | Tom  | 4   |
      <<[2].value>> | John | 19  |
      """

    Scenario: resolve input as object with index and group node with property and index
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }, {
        "value": {
          "name": "Jerry",
          "age": 2
        }
      }]
      """
      Then the following verification should pass:
      """
      :                  | name  | age |
                       0 | Tom   | 4   |
      <<[1], [2].value>> | Jerry | 2   |
      """

    Scenario: resolve input as object with key
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      :      | name  | age |
      get[0] | Tom   | 4   |
      """

  Rule: list mapping

    Scenario: resolve input as object when has list mapping key
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 4
      }, {
        "name": "Jerry",
        "age": 2
      }]
      """
      Then the following verification should pass:
      """
      :       | 0   | 1     |
       name[] | Tom | Jerry |
       age[]  | 4   | 2     |
      """
