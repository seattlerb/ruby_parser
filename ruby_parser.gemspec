# encoding: utf-8

$:.unshift File.expand_path('../lib', __FILE__)
require 'ruby_parser'

Gem::Specification.new do |s|
  s.name         = "ruby_parser"
  s.version      = RubyParser::VERSION
  s.authors      = ["Bryan Helmkamp"]
  s.email        = "bryan@brynary.com"
  s.homepage     = "https://github.com/brynary/ruby_parser"
  s.summary      = "[summary]"
  s.description  = "[description]"

  s.files        = Dir["{lib/**/*,[A-Z]*}"]
  s.platform     = Gem::Platform::RUBY
  s.require_path = 'lib'
  s.rubyforge_project = '[none]'
end
