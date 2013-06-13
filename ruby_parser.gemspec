$LOAD_PATH.unshift File.expand_path("../lib", __FILE__)

# Maintain gem"s version
require "version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name = "ruby_parser"
  s.authors = ["Ryan Davis"]
  s.email = ""
  s.homepage = "https://github.com/seattlerb/ruby_parser"
  s.summary = "ruby_parser (RP) is a ruby parser written in pure ruby (utilizing racc--which does by default use a C extension)."
  s.description = "RP's output is the same as ParseTree's output: s-expressions using ruby's arrays and base types."
  s.version = RubyParser::VERSION

  s.files = Dir["{bin,lib}/**/*"] + ["Rakefile", "Readme.md"]

  s.add_development_dependency "minitest"
end
