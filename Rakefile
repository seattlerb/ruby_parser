# -*- ruby -*-

require 'rubygems'
require 'rake/testtask'
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

task :test => ["lib/ruby_parser.rb"]

Rake::TestTask.new do |t|
  t.libs << "test"
  t.test_files = FileList['test/test*.rb']
  t.verbose = true
end

rule '.rb' => '.y' do |t|
  sh "racc -c -v -g --no-extentions -o #{t.name} #{t.source}"
end

task :clean do
  rm_f(Dir["**/*~"] +
       Dir["**/*.diff"] +
       Dir["lib/ruby_parser.rb"] +
       Dir["lib/*.output"])
end

# vim: syntax=Ruby
