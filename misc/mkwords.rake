require "open-uri"
require "uri"
require "rake/clean"

IPADIC_URI = URI("http://mecab.googlecode.com/files/mecab-ipadic-2.7.0-20070801.tar.gz")
IPADIC_TAR_GZ = File.basename(IPADIC_URI.path)
IPADIC_TAR = File.basename(IPADIC_TAR_GZ, ".gz")
IPADIC_DIR = File.basename(IPADIC_TAR, ".tar")
WORDS_EUCJP_FILE = "words.euc-jp.txt"
WORDS_UTF8_FILE = "words.utf8.txt"
WORDS_SJIS_FILE = "words.sjis.txt"
WORDS_JIS_FILE = "words.jis.txt"

CLEAN.include(IPADIC_TAR_GZ)
CLEAN.include(IPADIC_TAR)
CLEAN.include(IPADIC_DIR)
CLOBBER.include(WORDS_EUCJP_FILE)
CLOBBER.include(WORDS_UTF8_FILE)
CLOBBER.include(WORDS_SJIS_FILE)
CLOBBER.include(WORDS_JIS_FILE)


file IPADIC_TAR_GZ do
  puts "Download #{IPADIC_URI}"
  open(IPADIC_TAR_GZ, "wb") do |w|
    open(IPADIC_URI, "rb") do |r|
      w.write r.read
    end
  end
end

file WORDS_EUCJP_FILE => [IPADIC_TAR_GZ] do
  unless Dir.exists?(IPADIC_DIR)
    sh "7za x #{IPADIC_TAR_GZ}"
    sh "7za x #{IPADIC_TAR}"
  end
  words = []
  Dir["#{IPADIC_DIR}/*.csv"].each do |csv|
    puts csv
    open(csv, "r:euc-jp") do |f|
      f.each do |line|
        words << line.split(/,/).first
      end
    end
  end
  puts "Generate #{WORDS_EUCJP_FILE}"
  open(WORDS_EUCJP_FILE, "w:euc-jp") do |w|
    w.puts words.uniq
  end
end
task :all => [WORDS_EUCJP_FILE]

[[WORDS_UTF8_FILE, "utf-8"], [WORDS_SJIS_FILE, "windows-31j"], [WORDS_JIS_FILE, "iso-2022-jp"]].each do |(filename, encoding)|
  file filename => [WORDS_EUCJP_FILE] do
    puts "Generate #{filename}"
    open(filename, "w:#{encoding}") do |w|
      open(WORDS_EUCJP_FILE, "r:euc-jp") do |r|
        r.each do |line|
          w.puts line rescue nil
        end
      end
    end
  end
  task :all => [filename]
end

task :default => [:all]
