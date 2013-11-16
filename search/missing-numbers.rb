result = Hash.new(0)
gets; A = gets.split(' ').map {|x| x.to_i }
gets; B = gets.split(' ').map {|x| x.to_i }
B.each { |x| result[x] += 1 }
A.each { |x| result[x] -= 1 }
result.delete_if {|key, value| value == 0 }
puts result.keys.sort.map{|x| x.to_s }.join(' ')
