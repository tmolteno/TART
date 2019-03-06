require 'RackServer'
require 'matrix'

n = 200

brdc = BrdcHandler.new
sp3 = Sp3Handler.new
 
t = Time.now().utc
puts t
# t = Time.new(2002, 6, 23, 21, 0, 0, "+00:00").utc

puts brdc.get_sv_position(t, 1)
puts sp3.get_sv_position(t, 1)

n.times do
  t = t + 1000.0
  bpos = Vector.elements(brdc.get_sv_position(t, 1))
  spos = Vector.elements(sp3.get_sv_position(t, 1))
  dr = (bpos - spos).norm()
  puts "#{bpos} #{spos} #{dr}"
end

puts brdc.get_sv_position(t, 1)
puts sp3.get_sv_position(t, 1)

