#include "noob.h"

int main(int argc, const char **argv) {
  noob_rebuild_yourself(argc, argv);

  noob_run("rm -rf ./src/klaus");
  noob_run("ocamlopt ./src/klaus.ml -o ./src/klaus");

  if (noob_has_flag(argc, argv, "show")) {
    noob_run("cat out.s");
  }
  if (noob_has_flag(argc, argv, "debug")) {
    noob_run("./src/klaus examples/test.kl");
  }

  return 0;
}
