require "ruby_parser/legacy/ruby_parser_extras"
require "racc/parser"

unless defined? RubyParser

  ##
  # RubyParser is a compound parser that uses all known versions to
  # attempt to parse.

  class RubyParser

    VERSIONS = []

    class Parser < Racc::Parser
      include ::LegacyRubyParserStuff

      def self.inherited x
        RubyParser::VERSIONS << x
      end

      def self.version
        Parser > self and self.name[/(?:V|Ruby)(\d+)/, 1].to_i
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
end

module RubyParserLegacy
  NEW_VERSIONS = ::RubyParser::VERSIONS.dup
end

require "ruby_parser/legacy/ruby18_parser"
require "ruby_parser/legacy/ruby19_parser"

class RubyParser # HACK
  VERSIONS.clear
  VERSIONS.concat RubyParserLegacy::NEW_VERSIONS

  class V19 < Ruby19Parser; end
  class V18 < Ruby18Parser; end
end
