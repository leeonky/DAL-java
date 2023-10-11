Feature: original-object

  Rule: meta property object access original java object

    Scenario: access basic java property
      Given the following json:
      """
      {
        "class": "key-value-class"
      }
      """
      Then the following verification should pass:
      """
      class: 'key-value-class' and
        ::object.class.simpleName= LinkedHashMap
      """

    Scenario: fallback to dal data property when no java property
      Given the following json:
      """
      {
        "field": "map-key-value"
      }
      """
      Then the following verification should pass:
      """
      ::object.field= map-key-value
      """

    Scenario: null value object is still null
      Given the following json:
      """
      null
      """
      Then the following verification should pass:
      """
      ::object= null
      """
