# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.plugin :seattlerb
Hoe.plugin :racc
Hoe.plugin :isolate

Hoe.add_include_dirs "../../sexp_processor/dev/lib"

Hoe.spec 'ruby_parser' do
  developer 'Ryan Davis', 'ryand-ruby@zenspider.com'

  self.rubyforge_name = 'parsetree'

  dependency 'sexp_processor', '~> 4.1'

  if plugin? :perforce then
    self.perforce_ignore << "lib/ruby18_parser.rb"
    self.perforce_ignore << "lib/ruby19_parser.rb"
  end

  self.racc_flags << " -t" if plugin?(:racc) && ENV["DEBUG"]
end

file "lib/ruby18_parser.rb" => "lib/ruby18_parser.y"
file "lib/ruby19_parser.rb" => "lib/ruby19_parser.y"

task :clean do
  rm_rf(Dir["**/*~"] +
        Dir["**/*.diff"] +
        Dir["coverage.info"] +
        Dir["coverage"] +
        Dir["lib/*.output"])
end

def next_num(glob)
  num = Dir[glob].max[/\d+/].to_i + 1
end

desc "Compares PT to RP and deletes all files that match"
task :compare do
  files = Dir["unit/**/*.rb"]
  puts "Parsing #{files.size} files"
  files.each do |file|
    puts file
    system "./cmp.rb -q #{file} && rm #{file}"
  end
  system 'find -d unit -type d -empty -exec rmdir {} \;'
end

desc "Compares PT to RP and stops on first failure"
task :find_bug do
  files = Dir["unit/**/*.rb"]
  puts "Parsing #{files.size} files"
  files.each do |file|
    puts file
    sh "./cmp.rb -q #{file}"
  end
end

task :sort do
  sh 'grepsort "^ +def" lib/ruby_lexer.rb'
  sh 'grepsort "^ +def (test|util)" test/test_ruby_lexer.rb'
end

task :loc do
  loc1  = `wc -l ../1.0.0/lib/ruby_lexer.rb`[/\d+/]
  flog1 = `flog -s ../1.0.0/lib/ruby_lexer.rb`[/\d+\.\d+/]
  loc2  = `cat lib/ruby_lexer.rb lib/ruby_parser_extras.rb | wc -l`[/\d+/]
  flog2 = `flog -s lib/ruby_lexer.rb lib/ruby_parser_extras.rb`[/\d+\.\d+/]

  loc1, loc2, flog1, flog2 = loc1.to_i, loc2.to_i, flog1.to_f, flog2.to_f

  puts "1.0.0: loc = #{loc1} flog = #{flog1}"
  puts "dev  : loc = #{loc2} flog = #{flog2}"
  puts "delta: loc = #{loc2-loc1} flog = #{flog2-flog1}"
end

desc "Validate against all normal files in unit dir"
task :validate do
  sh "./cmp.rb unit/*.rb"
end

def run_and_log cmd, prefix
  files = ENV['FILES'] || 'unit/*.rb'
  p, x = prefix, "txt"
  n = Dir["#{p}.*.#{x}"].map { |s| s[/\d+/].to_i }.max + 1 rescue 1
  f = "#{p}.#{n}.#{x}"

  sh "#{cmd} #{Hoe::RUBY_FLAGS} bin/ruby_parse -q -g #{files} &> #{f}"

  puts File.read(f)
end

desc "Benchmark against all normal files in unit dir"
task :benchmark do
  run_and_log "ruby", "benchmark"
end

desc "Profile against all normal files in unit dir"
task :profile do
  run_and_log "zenprofile", "profile"
end

desc "what was that command again?"
task :huh? do
  puts "ruby #{Hoe::RUBY_FLAGS} bin/ruby_parse -q -g ..."
end

task :irb => [:isolate] do
  sh "GEM_HOME=#{Gem.path.first} irb -rubygems -Ilib -rruby_parser;"
end

def (task(:phony)).timestamp
  Time.at 0
end

task :isolate => :phony

file "lib/ruby18_parser.rb" => :isolate
file "lib/ruby19_parser.rb" => :isolate

task :compare18 do
  sh "./yack.rb lib/ruby18_parser.output > racc18.txt"
  sh "./yack.rb parse18.output > yacc18.txt"
  sh "diff -du racc18.txt yacc18.txt || true"
  puts
  sh "diff -du racc18.txt yacc18.txt | wc -l"
end

task :compare19 do
  sh "./yack.rb lib/ruby19_parser.output > racc19.txt"
  sh "./yack.rb parse19.output > yacc19.txt"
  sh "diff -du racc19.txt yacc19.txt || true"
  puts
  sh "diff -du racc19.txt yacc19.txt | wc -l"
end

task :debug => :isolate do
  ENV["V"] ||= "19"
  Rake.application[:parser].invoke # this way we can have DEBUG set

  $: << "lib"
  require 'ruby_parser'
  require 'pp'

  parser = if ENV["V"] == "18" then
             Ruby18Parser.new
           else
             Ruby19Parser.new
           end

  time = (ENV["RP_TIMEOUT"] || 10).to_i

  file = ENV["F"] || ENV["FILE"]

  ruby = if file then
           File.read(file)
         else
           file = "env"
           ENV["R"] || ENV["RUBY"]
         end

  begin
    pp parser.process(ruby, file, time)
  rescue Racc::ParseError => e
    p e
    ss = parser.lexer.src
    src = ss.string
    lines = src[0..ss.pos].split(/\n/)
    abort "on #{file}:#{lines.size}"
  end
end

task :debug_ruby do
  file = ENV["F"] || ENV["FILE"]
  sh "ruby19 -cwy #{file} 2>&1 | ./yuck.rb"
end

task :extract => :isolate do
  ENV["V"] ||= "19"
  Rake.application[:parser].invoke # this way we can have DEBUG set

  file = ENV["F"] || ENV["FILE"]

  ruby "-Ilib", "bin/ruby_parse_extract_error", file
end

task :bugs do
  sh "for f in bug*.rb ; do rake19 debug F=$f && rm $f ; done"
end

# vim: syntax=Ruby
