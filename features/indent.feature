Feature: JSON config mode auto indentation
  In order to edit JSON efficiently
  As an Emacs user
  I want to indent code automatically

  Scenario: Indent after '{'
    Given the buffer is empty
    When I insert "{"
    And I press "C-j"
    Then current column should be 2

    When I insert ""name1": {"
    And I press "C-j"
    Then current column should be 4

    When I insert " "name2": {"
    And I press "C-j"
    Then current column should be 7

  Scenario: Indent before '{'
    Given the buffer is empty
    When I insert "{"
    And I place the cursor before "{"
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert "{  "name1": {"
    And I place the cursor after ": "
    And I press "C-j"
    Then current column should be 3

    Given the buffer is empty
    When I insert:
    """
    {
        "name": {
    """
    And I place the cursor after ": "
    And I press "C-j"
    Then current column should be 4

  Scenario: Indent after '['
    Given the buffer is empty
    When I insert "["
    And I press "C-j"
    Then current column should be 2

    When I insert "["
    And I press "C-j"
    Then current column should be 4

    When I insert " 123, ["
    And I press "C-j"
    Then current column should be 7

  Scenario: Indent before '['
    Given the buffer is empty
    When I insert "["
    And I place the cursor before "["
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert "[  123, ["
    And I place the cursor after ", "
    And I press "C-j"
    Then current column should be 3

    Given the buffer is empty
    When I insert:
    """
    [
        123, [
    """
    And I place the cursor after ", "
    And I press "C-j"
    Then current column should be 4

  Scenario: Indent after ','
    Given the buffer is empty
    When I insert "{ "name": true,"
    And I press "C-j"
    Then current column should be 2

    Given the buffer is empty
    When I insert "{  "name": true,"
    And I press "C-j"
    Then current column should be 3

    Given the buffer is empty
    When I insert:
    """
    {
        "name": true,
    """
    And I press "C-j"
    Then current column should be 4

    Given the buffer is empty
    When I insert "[ null,"
    And I press "C-j"
    Then current column should be 2

    Given the buffer is empty
    When I insert "[  null,"
    And I press "C-j"
    Then current column should be 3

    Given the buffer is empty
    When I insert:
    """
    [
        null,
    """
    And I press "C-j"
    Then current column should be 4

  Scenario: Indent before ','
    Given the buffer is empty
    When I insert "{ "name": false,"
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert:
    """
    {
      "name": false,
    """
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert "[ 12345,"
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert:
    """
    [
      12345,
    """
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0

  Scenario: Indent configuration
    Given the buffer is empty
    When I set jsonconfig-basic-offset to 4
    And I insert "{"
    And I press "C-j"
    Then current column should be 4

    And I insert "["
    And I press "C-j"
    Then current column should be 8

    Given the buffer is empty
    When I insert:
    """
    {
        "name": {
    """
    And I press "C-j"
    Then current column should be 8

    Given the buffer is empty
    When I insert:
    """
    [
        {
    """
    And I press "C-j"
    Then current column should be 8

    Given the buffer is empty
    When I insert:
    """
    {
        "name": null,
    """
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0

    Given the buffer is empty
    When I insert:
    """
    [
        12345,
    """
    And I place the cursor before ","
    And I press "C-j"
    Then current column should be 0
