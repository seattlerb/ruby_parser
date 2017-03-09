# -*- ruby -*-

$:.unshift "../../hoe/dev/lib"

require "rubygems"
require "hoe"

Hoe.plugin :seattlerb
Hoe.plugin :racc
Hoe.plugin :isolate
Hoe.plugin :rdoc

Hoe.add_include_dirs "../../sexp_processor/dev/lib"
Hoe.add_include_dirs "../../minitest/dev/lib"
Hoe.add_include_dirs "../../oedipus_lex/dev/lib"

V1   = %w[18 19]
V2   = %w[20 21 22 23]
V1_2 = V1 + V2

Hoe.spec "ruby_parser" do
  developer "Ryan Davis", "ryand-ruby@zenspider.com"

  license "MIT"

  dependency "sexp_processor", "~> 4.1"
  dependency "rake", "< 11", :developer
  dependency "oedipus_lex", "~> 2.1", :developer

  if plugin? :perforce then     # generated files
    V1_2.each do |n|
      self.perforce_ignore << "lib/ruby#{n}_parser.rb"
    end

    V2.each do |n|
      self.perforce_ignore << "lib/ruby#{n}_parser.y"
    end
  end

  self.racc_flags << " -t" if plugin?(:racc) && ENV["DEBUG"]
end

V2.each do |n|
  file "lib/ruby#{n}_parser.y" => "lib/ruby_parser.yy" do |t|
    puts n
    flags = V2.map { |m| c = n==m ? "D" : "U"; "-#{c}RUBY#{m}" }.join " "
    cmd = 'unifdef -tk %s -UDEAD %s > %s || true' % [flags, t.source, t.name]
    sh cmd
  end
end

V1_2.each do |n|
  file "lib/ruby#{n}_parser.rb" => "lib/ruby#{n}_parser.y"
end

file "lib/ruby_lexer.rex.rb" => "lib/ruby_lexer.rex"

task :clean do
  rm_rf(Dir["**/*~"] +
        Dir["diff.diff"] + # not all diffs. bit me too many times
        Dir["coverage.info"] +
        Dir["coverage"] +
        Dir["lib/ruby2*_parser.y"] +
        Dir["lib/*.output"])
end

task :sort do
  sh "grepsort '^ +def' lib/ruby_lexer.rb"
  sh "grepsort '^ +def (test|util)' test/test_ruby_lexer.rb"
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

# to create parseXX.output:
#
# 1) check out the XX version of ruby
# 2) Edit uncommon.mk, find the ".y.c" rule and remove the RM lines
# 3) run `rm -f parse.c; make parse.c`
# 4) run `bison -r all parse.tmp.y`
# 5) mv parse.tmp.output parseXX.output

# possibly new instructions:
#
# 1) check out the XX version of ruby
# 2) YFLAGS="-r all" make parse.c
# 3) mv y.output parseXX.output

V1_2.each do |v|
  diff    = "compare/diff#{v}.diff"
  rp_txt  = "compare/rp#{v}.txt"
  mri_txt = "compare/mri#{v}.txt"
  compare = "compare#{v}"

  file diff do
    Dir.chdir "compare" do
      sh "rake"
    end
  end

  task :compare => compare

  task compare => diff do
    cmd = "diff -du #{mri_txt} #{rp_txt}"
    sh "#{cmd} > #{diff} || true"
    sh "wc -l #{diff}"
  end
end

task :debug => :isolate do
  ENV["V"] ||= V1_2.last
  Rake.application[:parser].invoke # this way we can have DEBUG set
  Rake.application[:lexer].invoke # this way we can have DEBUG set

  $: << "lib"
  require "ruby_parser"
  require "pp"

  klass = Object.const_get("Ruby#{ENV["V"]}Parser") rescue nil
  raise "Unsupported version #{ENV["V"]}" unless klass
  parser = klass.new

  time = (ENV["RP_TIMEOUT"] || 10).to_i

  n = ENV["BUG"]
  file = (n && "bug#{n}.rb") || ENV["F"] || ENV["FILE"]

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
    ss = parser.lexer.ss
    src = ss.string
    lines = src[0..ss.pos].split(/\n/)
    abort "on #{file}:#{lines.size}"
  end
end

task :debug_ruby do
  file = ENV["F"] || ENV["FILE"]
  sh "/Users/ryan/Desktop/DVDs/debugparser/miniruby -cwy #{file} 2>&1 | ./yuck.rb"
end

task :extract => :isolate do
  ENV["V"] ||= V1_2.last
  Rake.application[:parser].invoke # this way we can have DEBUG set

  file = ENV["F"] || ENV["FILE"]

  ruby "-Ilib", "bin/ruby_parse_extract_error", file
end

task :bugs do
  sh "for f in bug*.rb ; do #{Gem.ruby} -S rake debug F=$f && rm $f ; done"
end

# vim: syntax=Ruby
