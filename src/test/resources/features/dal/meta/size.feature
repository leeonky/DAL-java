Feature: meta ::size

  Scenario: access list size
    Given the following json:
    """
      [1, 2]
    """
    When evaluate by:
    """
    ::size
    """
    Then the result should:
    """
    : 2
    """
    Then the inspect should:
    """
    ::size
    """

  Scenario: raise error when target is not list
    Given the following json:
    """
      {
        "data": "not list"
      }
    """
    When evaluate by:
    """
    data::size
    """
    Then failed with the message:
    """
    Invalid meta property `size` for: java.lang.String
    <not list>
    """
    And got the following notation:
    """
    data::size
          ^
    """

  Scenario: use size[] to mapping sub list size ot new list
    Given the following input data:
    """
      {"list": [[1,2], [1,2,3]]}
    """
    Then the following assertion should pass:
    """
      list::size[] = [2 3]
    """


#  TODO in table row / cell*******************