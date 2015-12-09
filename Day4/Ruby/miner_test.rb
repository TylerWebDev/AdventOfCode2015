require 'test/unit'
require_relative 'miner.rb'

# Tests the MinerClass Test
class MinerTest < Test::Unit::TestCase
  def test_lowest_success
    assert_equal 609_043, Miner.new.lowest_success('abcdef')
    assert_equal 1_038_736, Miner.new.lowest_success('bgvyzdsv', 6)
  end
end
