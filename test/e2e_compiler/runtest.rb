require 'shellwords'

base_path = File.dirname(File.expand_path('', __FILE__))
executable_path = File.join(base_path, '../../semicaml')
lib_path = File.join(base_path, '../../libsemiruntime.a')
samples_path = File.join(base_path, '../../samples')

results = []

Dir.glob(File.join(samples_path, "*.seml")).sort.each do |fname|
  basic_name = (File.basename fname, ".*")
  executable_f = Shellwords.escape (File.join base_path, "#{basic_name}.run")
  cmd = "#{executable_path} #{Shellwords.escape fname} --lib #{Shellwords.escape lib_path} -o #{executable_f} > /dev/null 2>&1"
  puts "Execute >>> #{cmd}"
  r = system cmd


  input_f = Shellwords.escape (File.join base_path, (File.join "testcases", "#{basic_name}.in"))
  output_f = Shellwords.escape (File.join base_path, (File.join "testcases", "#{basic_name}.out"))

  result = `#{executable_f} < #{input_f}`
  output = File.open(output_f, 'r').read

  if result == output && $?.exitstatus == 0
    results << [true, fname, nil, nil]
  else
    results << [false, fname, output, result]
  end
end

puts "=" * 60
puts " run #{results.length} cases"
puts "=" * 60

s = 0
f = 0

results.each.with_index(1) do |r, i|
  suc, fname, expect, result = r

  puts "#{if suc then "SUCCESS" else "FAILURE" end} (#{i}/#{results.length}) #{fname}"

  if suc
    s += 1
  else
    puts "expect:"
    puts expect
    puts ""
    puts "but, result is"
    puts result

    f += 1
  end

  puts ""
  puts "=" * 60
end

puts "result"
puts "SUCCEEDED : #{s} / #{results.length}"
puts "FAILED    : #{f} / #{results.length}"
puts "=" * 60
