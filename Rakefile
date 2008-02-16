# -*- ruby -*-

require 'rubygems'
require 'hoe'

$: << '../../ParseTree/dev/lib/'
require './lib/ruby_parser_extras.rb'

hoe = Hoe.new('ruby_parser', RubyParser::VERSION) do |parser|
  parser.rubyforge_name = 'parsetree'
  parser.developer('Ryan Davis', 'ryand-ruby@zenspider.com')
  parser.extra_deps << 'ParseTree'
end

hoe.spec.files += ['lib/ruby_parser.rb'] # jim.... cmon man

module Rake::TaskManager
  def all_tasks
    @tasks
  end
end

Rake.application.all_tasks["default"].prerequisites.clear

task :default => :parser
task :test => :parser

path = "pkg/ruby_parser-#{RubyParser::VERSION}"
task path => :parser do
  Dir.chdir path do
    sh "rake parser"
  end
end

task :parser => ["lib/ruby_parser.rb"]

rule '.rb' => '.y' do |t|
  sh "racc -l -t -o #{t.name} #{t.source}"
end

task :clean do
  rm_rf(Dir["**/*~"] +
        Dir["**/*.diff"] +
        Dir["coverage.info"] +
        Dir["coverage"] +
        Dir["lib/ruby_parser.rb"] +
        Dir["lib/*.output"])
end

def next_num(glob)
  num = Dir[glob].max[/\d+/].to_i + 1
end

def profile(type)
  num = next_num("profile_#{type}*.txt")
  sh "zenprofile -w -Ilib:ext:bin:test -rtest/unit test/test_ruby_#{type}.rb &> profile_#{type}_%03d.txt" % num
end

task :profile do
  profile 'lexer'
  profile 'parser'
end

begin
  require 'rcov/rcovtask'
  Rcov::RcovTask.new do |t|
    pattern = ENV['PATTERN'] || 'test/test_ruby_*.rb'

    t.test_files = FileList[pattern]
    t.verbose = true
    t.rcov_opts << "--threshold 80"
    t.rcov_opts << "--no-color"
  end
rescue LoadError
  # skip
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

task :rcov_info => :parser do
  pattern = ENV['PATTERN'] || "test/test_*.rb"
  ruby "-Ilib -S rcov --text-report --save coverage.info #{pattern}"
end

task :rcov_overlay do
  rcov, eol = Marshal.load(File.read("coverage.info")).last[ENV["FILE"]], 1
  puts rcov[:lines].zip(rcov[:coverage]).map { |line, coverage|
    bol, eol = eol, eol + line.length
    [bol, eol, "#ffcccc"] unless coverage
  }.compact.inspect
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

# vim: syntax=Ruby
