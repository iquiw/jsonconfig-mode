Feature: JSON config mode highlighting
  In order to increase JSON code visibility
  As an Emacs user
  I want to highlight JSON code

  Scenario: Highlight for keywords
    Given the buffer is empty
    When I insert:
    """
    [ true
    , false
    , null
    ]
    """
    And I go to word "true"
    Then current point should have the font-lock-keyword-face face
    When I go to word "false"
    Then current point should have the font-lock-keyword-face face
    When I go to word "null"
    Then current point should have the font-lock-keyword-face face

  Scenario: Highlight for numbers
    Given the buffer is empty
    When I insert:
    """
    [ 123
    , -456
    , 78.9
    , 987e6
    ]
    """
    And I go to word "123"
    Then current point should have the font-lock-constant-face face
    When I place the cursor before "456"
    Then current point should have the font-lock-constant-face face
    When I place the cursor after "78"
    Then current point should have the font-lock-constant-face face
    When I place the cursor after "987"
    Then current point should have the font-lock-constant-face face

  Scenario: Highlight for property names
    Given the buffer is empty
    When I insert:
    """
    { "abc": 1,
    , "cd\"\\e": null,
      "efg": false,
      "\"": 3
    , "": 4
    ]
    """
    And I go to word "abc"
    Then current point should have the font-lock-variable-name-face face
    And I go to word "e"
    Then current point should have the font-lock-variable-name-face face
    And I go to word "efg"
    Then current point should have the font-lock-variable-name-face face
    When I place the cursor before ""\"""
    Then current point should have the font-lock-variable-name-face face
    When I place the cursor before """"
    Then current point should have the font-lock-variable-name-face face

  Scenario: Highlight for strings
    Given the buffer is empty
    When I insert:
    """
    { "abc": "123"
    , "cd\"\\e": ["CDE"]
    }
    """
    And I go to word "123"
    Then current point should have the font-lock-string-face face
    And I go to word "CDE"
    Then current point should have the font-lock-string-face face

  Scenario: No highlight
    Given the buffer is empty
    When I insert:
    """
    { "abc": "123"
    , "cd\"\\e": ["CDE"]
    }
    """
    And I place the cursor before "{"
    Then current point should have no face
    And I place the cursor before ","
    Then current point should have no face
    And I place the cursor before ":"
    Then current point should have no face
    And I place the cursor before "["
    Then current point should have no face
