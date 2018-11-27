require "ruby_parser_extras"
require "racc/parser"

##
# RubyParser is a compound parser that uses all known versions to
# attempt to parse.

class RubyParser

  VERSIONS = []

  class Parser < Racc::Parser
    include RubyParserStuff

    def self.inherited x
      RubyParser::VERSIONS << x
    end

    def self.version= v
       @version = v
    end

    def self.version
      @version ||= Parser > self && self.name[/(?:V|Ruby)(\d+)/, 1].to_i
    end
  end

  class SyntaxError < RuntimeError; end

  def process s, f = "(string)", t = 10
    e = nil
    VERSIONS.each do |klass|
      parser = klass.new
      begin
        return parser.process s, f, t
      rescue Racc::ParseError, RubyParser::SyntaxError => exc
        e = exc
      end
    end
    raise e
  end

  alias :parse :process

  def reset
    # do nothing
  end

  def self.latest
    VERSIONS.first.new
  end

  def self.for_current_ruby
    name  = "V#{RUBY_VERSION[/^\d+\.\d+/].delete "."}"
    klass = if const_defined? name then
              const_get name
            else
              latest = VERSIONS.first
              warn "NOTE: RubyParser::#{name} undefined, using #{latest}."
              latest
            end

    klass.new
  end
end

##
# Unfortunately a problem with racc is that it won't let me namespace
# properly, so instead of RubyParser::V18, I still have to generate
# the old Ruby23Parser and shove it in as V23.

require "ruby18_parser"
require "ruby19_parser"
require "ruby20_parser"
require "ruby21_parser"
require "ruby22_parser"
require "ruby23_parser"
require "ruby24_parser"
require "ruby25_parser"

class RubyParser # HACK
  VERSIONS.clear # also a HACK caused by racc namespace issues

  class V25 < ::Ruby25Parser; end
  class V24 < ::Ruby24Parser; end
  class V23 < ::Ruby23Parser; end
  class V22 < ::Ruby22Parser; end
  class V21 < ::Ruby21Parser; end
  class V20 < ::Ruby20Parser; end
  class V19 < ::Ruby19Parser; end
  class V18 < ::Ruby18Parser; end
end
