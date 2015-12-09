require 'digest'

# mines "AdventCoins"
class Miner
  def initialize
  end

  def lowest_success(secret, difficulty = 5)
    i = 0
    diff_target = '0' * difficulty
    i += 1 until Digest::MD5.hexdigest(secret + i.to_s).start_with? diff_target
    i
  end
end

# Solution to part 1 of the problem
# puts Miner.new().lowest_success('bgvyzdsv')

# Solution to part 2 of the problem
# puts Miner.new().lowest_success('bgvyzdsv', 6)
