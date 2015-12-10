# calculates the amount of paper and/or ribbon
class WrappingCalculator
  attr_reader :dimensions
  attr_reader :input_file

  def initialize(input_file)
    @input_file = input_file
    @dimensions = format_packages_list
  end

  def total_paper_needed
    total = 0
    dimensions.each do |gift|
      total += wrapping_for_gift(gift)
    end
    total
  end

  def wrapping_for_gift(gift)
    # 2*l*w + 2*w*h + 2*h*l
    top = gift[:length] * gift[:width]
    front = gift[:width] * gift[:height]
    side = gift[:height] * gift[:length]
    slack = smallest_side(gift)

    2 * top + 2 * front + 2 * side + slack
  end

  def total_ribbon_needed
    total = 0
    dimensions.each do |gift|
      total += total_ribbon_for_gift(gift)
    end
    total
  end

  def ribbon_for_gift(gift)
    sides = gift.values.sort[0..1]
    sides[0] * 2 + sides[1] * 2
  end

  def total_ribbon_for_gift(gift)
    ribbon_for_gift(gift) + bow_for_gift(gift)
  end

  def bow_for_gift(gift)
    gift[:length] * gift[:width] * gift[:height]
  end

  def smallest_side(gift)
    ss = gift.values.sort[0..1]
    ss[0] * ss[1]
  end

  private

  def format_packages_list
    File.read(input_file).split("\n").each.map do |a|
      tmp = a.split('x')
      { length: tmp[0].to_i, width: tmp[1].to_i, height: tmp[2].to_i }
    end
  end
end

wp = WrappingCalculator.new('gifts.txt')
puts wp.total_ribbon_needed
