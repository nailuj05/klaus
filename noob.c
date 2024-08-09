#include "noob.h"

int main(int argc, const char **argv) {
  RebuildYourself(argc, argv);

  BuildAndRunCommand("rm -rf ./src/klaus");
  BuildAndRunCommand("ocamlc ./src/klaus.ml -o ./src/klaus");

  if (HasFlag(argc, argv, "run")) {
    BuildAndRunCommand("./src/klaus");
  }

  return 0;
}
