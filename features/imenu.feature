Feature: JSON config mode imenu
  In order to navigate top level properties
  As an Emacs user
  I want to see properties in imenu

  Scenario: Imenu for object
    Given the buffer is empty
    When I insert:
    """
    { "top1": 1
    , "top2": 2
    , "top3": {
        "sub1": 1,
        "sub2": 2
      }
    }
    """
    And I call jsonconfig-create-imenu-index
    Then imenu entries are:
    """
    (("Variables"
      ("top1" . 3)
      ("top2" . 15)
      ("top3" . 27)))
    """

  Scenario: No imenu for other types
    Given the buffer is empty
    When I insert:
    """
    [ "top1", 1
    , "top2", 2
    , "top3": {
        "sub1": 1,
        "sub2": 2
      }
    ]
    """
    And I call jsonconfig-create-imenu-index
    Then imenu entry none

    Given the buffer is empty
    When I insert:
    """
    "test"
    """
    And I call jsonconfig-create-imenu-index
    Then imenu entry none

    Given the buffer is empty
    When I insert:
    """
    true
    """
    And I call jsonconfig-create-imenu-index
    Then imenu entry none

    Given the buffer is empty
    When I insert:
    """
    123
    """
    And I call jsonconfig-create-imenu-index
    Then imenu entry none
