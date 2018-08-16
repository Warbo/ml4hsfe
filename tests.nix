{ jq, ML4HSFE, runCommand }:

{
  outer-loop = runCommand "ml4hsfe-outer-loop-test"
    {
      buildInputs = [ jq ML4HSFE ];
      examples    = ./examples;
    }
    ''
      for EX in "$examples"/ml4hsfe-outer-loop-example-input*.json
      do
        export WIDTH=10
        export HEIGHT=10
        HASKELL_RESULT=$(ml4hsfe-outer-loop < "$EX") || {
          echo "Failed sending '$EX' through ml4hsfe-outer-loop" 1>&2
          exit 1
        }

        echo "Ensuring Haskell results contain clusters" 1>&2
        RESULT=$(echo "$HASKELL_RESULT" | jq 'map(has("cluster")) | all')
        [[ "x$RESULT" = "xtrue" ]] || {
          echo "Haskell results missing clusters" 1>&2
          exit 1
        }
      done

      mkdir "$out"
    '';
}
