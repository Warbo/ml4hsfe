# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir  ? ./.., # Path to the revision containing the benchmarks
  root ? ./.., # Path to the revision being benchmarked
  ...
}:

with builtins;
with {
  fixed    = import "${dir }/overlayed.nix";
  measured = import "${root}/overlayed.nix";
};

# Use 'paths' and 'vars' to pass things from 'measured' to the benchmark scripts
fixed.mkBin {
  name   = "python";
  paths  = with fixed; [ (python3.withPackages (p: [])) ];
  vars   = { profiled = "${measured.memoryProfile}/heap.hp"; };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
