# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/ruby_parser_extras.rb'

hoe = Hoe.new('ruby_parser', RubyParser::VERSION) do |p|
  p.rubyforge_name = 'parsetree'
  p.author = 'Ryan Davis'
  p.email = 'ryand-ruby@zenspider.com'
  p.summary = p.paragraphs_of('README.txt', 2).join("\n\n")
  p.description = p.paragraphs_of('README.txt', 2..6).join("\n\n")
  p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[-1]
  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")
  p.extra_deps << 'ParseTree'
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
  sh "racc -g -o #{t.name} #{t.source}"
end

task :clean do
  rm_f(Dir["**/*~"] +
       Dir["**/*.diff"] +
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

# vim: syntax=Ruby
