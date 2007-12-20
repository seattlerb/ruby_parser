# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/ruby_lexer.rb'

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

# require 'rcov/rcovtask'
# Rcov::RcovTask.new do |t|
#   t.test_files = FileList['test/test_ruby_lexer.rb']
# end

# vim: syntax=Ruby
