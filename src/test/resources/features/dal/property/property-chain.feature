Feature: property

  Scenario: property chain
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }]
      }
    """
    When evaluate by:
    """
      items[0].id
    """
    Then the result should:
    """
    : 100
    """
    And the inspect should:
    """
    items[0].id
    """

  Scenario: Map list element
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }, {
          "id": 200
        }]
      }
    """
    When evaluate by:
    """
      items.id[]
    """
    Then the result should:
    """
    : [100 200]
    """
    And the inspect should:
    """
    items.id[]
    """

  Scenario: List mapping chain
    Given the following json:
    """
      {
        "products": [{
          "catalog": {
            "sub": {
              "value": {
                "string": "001"
              }
            }
          }
        }, {
          "catalog": {
            "sub": {
              "value": {
                "string": "002"
              }
            }
          }
        }]
      }
    """
    When evaluate by:
    """
      products.catalog[].sub.value.string
    """
    Then the result should:
    """
    : ["001" "002"]
    """
    And the inspect should:
    """
    products.catalog[].sub.value.string
    """

# TODO
  Scenario: disable list auto mapping
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }]
      }
    """
    When evaluate by:
    """
    items.id= [100]
    """
#    Then failed with the message:
#    """
#    Method not found
#    """
#    And got the following notation:
#    """
#    items.id= [100]
#          ^
#    """
