require 'rake/clean'
CLEAN.include("**/*.hi")
CLEAN.include("**/*.o")
CLEAN.include("gol")

file "gol" => ["gol.hs"] do
  sh "ghc gol"
end

task :run do
  Rake::Task["gol"].invoke
  sh "./gol"
end

task :default => [:run]
