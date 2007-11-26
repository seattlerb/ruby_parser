# -*- ruby -*-

require 'rubygems'
require 'hoe'
require './lib/ruby_lexer.rb'

Hoe.new('RubyParser', RubyParser::VERSION) do |p|
  p.rubyforge_name = 'ruby_parser'
  p.author = 'Ryan Davis'
  p.email = 'ryand-ruby@zenspider.com'
  p.summary = 'FIX'
  p.description = p.paragraphs_of('README.txt', 2..5).join("\n\n")
  p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1]
  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")
end

module Rake::TaskManager
  def all_tasks
    @tasks
  end
end

Rake.application.all_tasks["default"].prerequisites.clear

task :default => :parser

task :test => :parser

task :parser => ["lib/ruby_parser.rb"]

rule '.rb' => '.y' do |t|
  sh "racc -c -v -g --no-extentions -o #{t.name} #{t.source}"
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
