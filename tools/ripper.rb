#!/usr/bin/env ruby -ws

$d ||= false
$p ||= false

require "ripper/sexp"
require "pp" if $p

if ARGV.empty? then
  warn "reading from stdin"
  ARGV << "-"
end

ARGV.each do |path|
  src = path == "-" ? $stdin.read : File.read(path)
  rip = Ripper::SexpBuilderPP.new src
  rip.yydebug = $d

  sexp = rip.parse

  puts "accept" unless rip.error?

  if $p then
    pp sexp
  else
    p sexp
  end
end
