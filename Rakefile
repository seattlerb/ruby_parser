# -*- ruby -*-

$:.unshift "../../hoe/dev/lib"

require "rubygems"
require "hoe"

Hoe.plugin :seattlerb
Hoe.plugin :racc
Hoe.plugin :isolate
Hoe.plugin :rdoc

Hoe.add_include_dirs "../../sexp_processor/dev/lib"
Hoe.add_include_dirs "../../minitest/dev/lib"
Hoe.add_include_dirs "../../oedipus_lex/dev/lib"

V1   = %w[18 19]
V2   = %w[20 21 22 23 24]
V1_2 = V1 + V2

Hoe.spec "ruby_parser" do
  developer "Ryan Davis", "ryand-ruby@zenspider.com"

  license "MIT"

  dependency "sexp_processor", "~> 4.9"
  dependency "rake", "< 11", :developer
  dependency "oedipus_lex", "~> 2.5", :developer

  if plugin? :perforce then     # generated files
    V1_2.each do |n|
      self.perforce_ignore << "lib/ruby#{n}_parser.rb"
    end

    V2.each do |n|
      self.perforce_ignore << "lib/ruby#{n}_parser.y"
    end

    self.perforce_ignore << "lib/ruby_lexer.rex.rb"
  end

  if plugin?(:racc)
    self.racc_flags << " -t" if ENV["DEBUG"]
    self.racc_flags << " --superclass RubyParser::Parser"
    # self.racc_flags << " --runtime ruby_parser" # TODO: broken in racc
  end
end

V2.each do |n|
  file "lib/ruby#{n}_parser.y" => "lib/ruby_parser.yy" do |t|
    cmd = 'unifdef -tk -DV=%s -UDEAD %s > %s || true' % [n, t.source, t.name]
    sh cmd
  end
end

V1_2.each do |n|
  file "lib/ruby#{n}_parser.rb" => "lib/ruby#{n}_parser.y"
end

file "lib/ruby_lexer.rex.rb" => "lib/ruby_lexer.rex"

task :clean do
  rm_rf(Dir["**/*~"] +
        Dir["diff.diff"] + # not all diffs. bit me too many times
        Dir["coverage.info"] +
        Dir["coverage"] +
        Dir["lib/ruby2*_parser.y"] +
        Dir["lib/*.output"])
end

task :sort do
  sh "grepsort '^ +def' lib/ruby_lexer.rb"
  sh "grepsort '^ +def (test|util)' test/test_ruby_lexer.rb"
end

desc "what was that command again?"
task :huh? do
  puts "ruby #{Hoe::RUBY_FLAGS} bin/ruby_parse -q -g ..."
end

task :irb => [:isolate] do
  sh "GEM_HOME=#{Gem.path.first} irb -rubygems -Ilib -rruby_parser;"
end

def (task(:phony)).timestamp
  Time.at 0
end

task :isolate => :phony

def in_compare
  Dir.chdir "compare" do
    yield
  end
end

def dl v
  dir = v[/^\d+\.\d+/]
  url = "https://cache.ruby-lang.org/pub/ruby/#{dir}/ruby-#{v}.tar.bz2"
  path = File.basename url
  unless File.exist? path then
    system "curl -O #{url}"
  end
end

def ruby_parse version
  v         = version[/^\d+\.\d+/].delete "."
  rp_txt    = "rp#{v}.txt"
  mri_txt   = "mri#{v}.txt"
  parse_y   = "parse#{v}.y"
  tarball   = "ruby-#{version}.tar.bz2"
  ruby_dir  = "ruby-#{version}"
  diff      = "diff#{v}.diff"
  rp_out    = "lib/ruby#{v}_parser.output"

  c_diff    = "compare/#{diff}"
  c_rp_txt  = "compare/#{rp_txt}"
  c_mri_txt = "compare/#{mri_txt}"
  c_parse_y = "compare/#{parse_y}"
  c_tarball = "compare/#{tarball}"

  file tarball do
    in_compare do
      dl version
    end
  end

  file c_parse_y => c_tarball do
    in_compare do
      system "tar yxf #{tarball} #{ruby_dir}/{id.h,parse.y,tool/{id2token.rb,vpath.rb}}"
      Dir.chdir ruby_dir do
        if File.exist? "tool/id2token.rb" then
          sh "ruby tool/id2token.rb --path-separator=.:./ id.h parse.y > ../#{parse_y}"
        else
          cp "parse.y", "../#{parse_y}"
        end
      end
      sh "rm -rf #{ruby_dir}"
    end
  end

  file c_mri_txt => c_parse_y do
    in_compare do
      sh "bison -r all #{parse_y}"
      sh "./normalize.rb parse#{v}.output > #{mri_txt}"
      rm ["parse#{v}.output", "parse#{v}.tab.c"]
    end
  end

  file rp_out => :parser

  file c_rp_txt => rp_out do
    in_compare do
      sh "./normalize.rb ../#{rp_out} > #{rp_txt}"
    end
  end

  compare = "compare#{v}"

  desc "Compare all grammars to MRI"
  task :compare => compare

  task c_diff => [c_mri_txt, c_rp_txt] do
    in_compare do
      system "diff -du #{mri_txt} #{rp_txt} > #{diff}"
    end
  end

  desc "Compare #{v} grammar to MRI #{version}"
  task compare => c_diff do
    in_compare do
      system "wc -l #{diff}"
    end
  end

  task :clean do
    rm_f Dir[c_parse_y, c_mri_txt, c_rp_txt]
  end

  task :realclean do
    rm_f Dir[tarball]
  end
end

ruby_parse "1.8.7-p374"
ruby_parse "1.9.3-p551"
ruby_parse "2.0.0-p648"
ruby_parse "2.1.9"
ruby_parse "2.2.6"
ruby_parse "2.3.3"
# TODO ruby_parse "2.4.0"

task :debug => :isolate do
  ENV["V"] ||= V1_2.last
  Rake.application[:parser].invoke # this way we can have DEBUG set
  Rake.application[:lexer].invoke # this way we can have DEBUG set

  $: << "lib"
  require "ruby_parser"
  require "pp"

  klass = Object.const_get("Ruby#{ENV["V"]}Parser") rescue nil
  raise "Unsupported version #{ENV["V"]}" unless klass
  parser = klass.new

  time = (ENV["RP_TIMEOUT"] || 10).to_i

  n = ENV["BUG"]
  file = (n && "bug#{n}.rb") || ENV["F"] || ENV["FILE"]

  ruby = if file then
           File.read(file)
         else
           file = "env"
           ENV["R"] || ENV["RUBY"]
         end

  begin
    pp parser.process(ruby, file, time)
  rescue Racc::ParseError => e
    p e
    ss = parser.lexer.ss
    src = ss.string
    lines = src[0..ss.pos].split(/\n/)
    abort "on #{file}:#{lines.size}"
  end
end

task :debug_ruby do
  file = ENV["F"] || ENV["FILE"]
  sh "/Users/ryan/Desktop/DVDs/debugparser/miniruby -cwy #{file} 2>&1 | ./yuck.rb"
end

task :extract => :isolate do
  ENV["V"] ||= V1_2.last
  Rake.application[:parser].invoke # this way we can have DEBUG set

  file = ENV["F"] || ENV["FILE"]

  ruby "-Ilib", "bin/ruby_parse_extract_error", file
end

task :bugs do
  sh "for f in bug*.rb ; do #{Gem.ruby} -S rake debug F=$f && rm $f ; done"
end

# vim: syntax=Ruby
