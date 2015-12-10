require 'test-unit'

# calculates delivery data based on instructions
class Delivery
  def initialize(instructions)
    @instructions = instructions
    @x = 0
    @y = 0
    @houses_visited = [[0, 0]]
    @num_houses_visited = 1
    parse_instuctions
  end

  def parse_instuctions
    @instructions.split('').each do |instruction|
      case instruction
      when '>' then @x += 1
      when '<' then @x -= 1
      when '^' then @y -= 1
      when 'v' then @y += 1
      end
      increment_houses_visited([@x, @y])
    end
  end

  def increment_houses_visited(current_house)
    return false if @houses_visited.include? current_house
    @houses_visited << current_house
    @num_houses_visited += 1
  end

  def num_houses
    @num_houses_visited
  end
end

# tests the delivery class
class DeliveryTest < Test::Unit::TestCase
  def test_number_of_deliveries
    assert_equal(6, Delivery.new('^>v<<<').num_houses)
  end
end

# get the solution to problem 1
# elf_instructions = File.read('directions.txt')
# puts DeliveryInstructions.new(elf_instructions).num_houses

# calculates robot delivery data based on instructions
class RoboDelivery < Delivery
  def initialize(instructions)
    @santa = { x: 0, y: 0 }
    @robo_santa = { x: 0, y: 0 }
    super
  end

  def parse_instuctions
    @instructions.split('').each_with_index do |instruction, i|
      which_santa = i.even? ? @santa : @robo_santa

      case instruction
      when '>' then  which_santa[:x] += 1
      when '<' then  which_santa[:x] -= 1
      when '^' then  which_santa[:y] -= 1
      when 'v' then  which_santa[:y] += 1
      end
      increment_houses_visited([which_santa[:x], which_santa[:y]])
    end
  end
end

# tests the robo delivery class
class RoboDeliveryTest < Test::Unit::TestCase
  def test_number_of_deliveries
    assert_equal(3, RoboDelivery.new('^v').num_houses)
    assert_equal(3, RoboDelivery.new('^>v<').num_houses)
    assert_equal(11, RoboDelivery.new('^v^v^v^v^v').num_houses)
  end
end

# get the solution to problem 1
# elf_instructions ||= File.read('directions.txt')
# puts RoboDelivery.new(elf_instructions).num_houses
