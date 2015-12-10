# Judges a line seperated list (string) of words for niceness
class StringJudge
  def judge_input(input, opts = { legacy: false })
    nice_strings_count = 0
    input.split("\n").each do |word|
      nice_strings_count += 1 if nice?(word, opts[:legacy])
    end
    nice_strings_count
  end

  private

  def nice?(str, legacy)
    if legacy
      repeated_letters?(str) && !blacklisted_strings?(str) && three_vowels?(str)
    else
      letter_pair?(str) && letter_sandwhich?(str)
    end
  end

  def letter_pair?(str)
    str.split('').each_with_index.any? do |_letter, i|
      target = str[i..i.next]
      target.length == 2 && str.sub(target, ' ').include?(target)
    end
  end

  def letter_sandwhich?(str)
    str.split('').any? do |letter, _i|
      str =~ (/#{letter}.#{letter}/)
    end
  end

  def three_vowels?(str)
    str.scan(/[aeiou]/).count > 2
  end

  def repeated_letters?(str)
    str.split('').any? do |letter|
      str.include? letter * 2
    end
  end

  def blacklisted_strings?(str)
    %w(ab cd pq xy).any? { |pattern| str.include? pattern }
  end
end

# input = File.read('nice_or_naughty_strings.txt')
# output solution to part 1
# puts StringJudger.new.judge_input(input, legacy: true)

# output solution to part 2
# puts StringJudger.new.judge_input(input)
