require "strscan"

class RPStringScanner < StringScanner
  if ENV["DEBUG"] || ENV["TALLY"] then
    alias :old_getch :getch
    def getch
      c = self.old_getch
      where = caller.drop_while { |s| s =~ /(getch|nextc).$/ }.first
      where = where.split(/:/).first(2).join(":")
      if ENV["TALLY"] then
        d getch:where
      else
        d getch:[c, where]
      end
      c
    end

    alias :old_scan :scan
    def scan re
      s = old_scan re
      where = caller.drop_while { |x| x =~ /scan.$/ }.first
      where = where.split(/:/).first(2).join(":")
      if ENV["TALLY"] then
        d scan:[where]
      else
        d scan:[s, where] if s
      end
      s
    end

    def d o
      STDERR.puts o.inspect
    end
  end
end
