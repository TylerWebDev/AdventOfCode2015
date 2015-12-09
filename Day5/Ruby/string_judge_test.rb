require 'test/unit'
require_relative 'string_judge.rb'
require 'pry'

# test the string judger class
class StringJudgerTest < Test::Unit::TestCase
  def test_input
    # input is examples from the instructions + one edge case
    input = "qjhvhtzxzqqjkmpb\nxxyxx\nuurcxstgmygtbstg\nieodomkazucvgmuy\nmmggfwapsetemiuj"
    assert_equal 2, StringJudge.new.judge_input(input)
  end

  def test_input_legacy
    input = "aaa\nab" # shortest nice followed by shortest naughty - legacy
    assert_equal 1, StringJudge.new.judge_input(input, legacy: true)
  end
end
